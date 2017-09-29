{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Command.PrintAst where

import Control.Monad
import Language.PureScript as Ps
import Command.Compile as Compiler
import qualified Options.Applicative as Opts

-- TODO: cleanup/refactor. 
--       ... but I need to figure out the IO (), 'do' etc. stuff first
printAst :: PSCMakeOptions -> IO ()
printAst Compiler.PSCMakeOptions{..} = do
    fileNames <- globWarningOnMisses (unless pscmJSONErrors . warnFileTypeNotFound) pscmInput
    fileContents <- Compiler.readInput fileNames
    case Ps.lex (fst (head fileContents)) (snd (head fileContents)) of
        Left  err    -> print err        
        Right tokens -> case runTokenParser (fst (head fileContents)) parseModule tokens of
            Left err -> print err
            Right m  -> do
                print m
                -- TODO: generate KAST
                putStrLn ""

command :: Opts.Parser (IO ())
command = printAst <$> (Opts.helper <*> pscMakeOptions)