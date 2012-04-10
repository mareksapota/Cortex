{-# LANGUAGE FlexibleContexts #-}

module Cortex.Common.OptParse
    ( CmdSwitch
    , CmdName
    , CmdHelp
    , CmdArgs
    , CmdParsedArgs

    , empty
    , addOption
    , parseArgs
    , evalArgs

    , getOption
    , getOptionWithDefault
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad (forM_, liftM)
import Control.Monad.Error (MonadError, throwError)

import Cortex.Common.MaybeRead

-----

type CmdSwitch = String
type CmdName = String
type CmdHelp = String
type CmdArgs = Map [CmdSwitch] (CmdName, CmdHelp)
-- This map can actually be used to search for switches.
type CmdWorkArgs = Map CmdSwitch CmdName
type CmdParsedArgs = Map CmdName String

-- Adding options is pure.

empty :: CmdArgs
empty = Map.empty

addOption :: [CmdSwitch] -> CmdName -> CmdHelp -> CmdArgs -> CmdArgs
addOption [] _ _ options = options
addOption switches name help options =
    Map.insert (List.sort switches) (name, help) options

-- Parsing arguments can throw errors.

evalArgs :: (MonadError String m, MonadIO m) => CmdArgs -> m CmdParsedArgs
evalArgs options = do
    parsedArgs <- parseArgs options
    return $ snd parsedArgs

parseArgs :: (MonadError String m, MonadIO m) =>
    CmdArgs -> m ([String], CmdParsedArgs)
parseArgs options = do
    args <- liftIO getArgs
    parseArgs' options (flatten options) [] Map.empty args

flatten :: CmdArgs -> CmdWorkArgs
flatten options = flatten' (Map.toList options) Map.empty

flatten' :: [([CmdSwitch], (CmdName, CmdHelp))] ->
    CmdWorkArgs -> CmdWorkArgs

flatten' [] workArgs = workArgs
flatten' (([], _):otherArgs) workArgs = flatten' otherArgs workArgs

flatten' (((switch:otherSwitches), (name, help)):otherArgs) workArgs =
    flatten' (((otherSwitches), (name, help)):otherArgs)
        (Map.insert switch name workArgs)

parseArgs' :: (MonadError String m, MonadIO m) => CmdArgs -> CmdWorkArgs ->
    [String] -> CmdParsedArgs -> [String] -> m ([String], CmdParsedArgs)
-- No arguments left to parse.
parseArgs' _ _ unknown parsedArgs [] = return (reverse unknown, parsedArgs)
-- Parse a new argument.
parseArgs' options workOptions unknown parsedArgs (arg:args)
    | arg == "--help" || arg == "-h" = printHelp options
    | Map.member arg workOptions = do
        (a, pa) <- saveArg (workOptions Map.! arg) args parsedArgs
        parseArgs' options workOptions unknown pa a
    | otherwise = parseArgs' options workOptions (arg:unknown) parsedArgs args

saveArg :: (MonadError String m) =>
    CmdName -> [String] -> CmdParsedArgs -> m ([String], CmdParsedArgs)
saveArg _ [] _ = throwError "Expected a value after the switch."
saveArg name (arg:args) parsedArgs = return $
    (args, Map.insert name arg parsedArgs)

-- Help printing functions.

printHelp :: (MonadError String m, MonadIO m) => CmdArgs -> m a
printHelp options = do
    name <- liftIO getProgName
    liftIO $ putStrLn $ "usage: " ++ name ++ " [options]"
    liftIO $ putStrLn $ "  --help, -h\t\tPrint help and exit"
    forM_ (Map.toAscList options) printOption
    liftIO exitSuccess

printOption :: (MonadError String m, MonadIO m) =>
    ([CmdSwitch], (CmdName, CmdHelp)) -> m ()
printOption (switches, (_, help)) = do
    switchString <- makeSwitchString switches
    liftIO $ putStrLn $ "  " ++ switchString ++ "\t\t" ++ help

makeSwitchString :: (MonadError String m) => [CmdSwitch] -> m String
makeSwitchString [] = throwError "Option with no switches."
makeSwitchString [switch] = return switch
makeSwitchString (switch:others) = return $ switch ++ (makeSwitchString' others)

makeSwitchString' :: [CmdSwitch] -> String
makeSwitchString' [] = ""
makeSwitchString' (switch:others) =
    ", " ++ switch ++ (makeSwitchString' others)

-- Functions that deal with parsed args.

getOption :: (MonadError String m, MaybeRead a) =>
    CmdParsedArgs -> CmdName -> m (Maybe a)
getOption parsedArgs name = getOption' (Map.lookup name parsedArgs)

getOption' :: (MonadError String m, MaybeRead a) =>
    Maybe String -> m (Maybe a)
getOption' Nothing = return Nothing
getOption' (Just a) = do
    let v = maybeRead a
    case v of
        Nothing -> throwError "Argument has wrong type"
        Just _ -> return v

getOptionWithDefault :: (MonadError String m, MaybeRead a) =>
    CmdParsedArgs -> CmdName -> a -> m a
getOptionWithDefault args name defaultValue =
    liftM (maybe defaultValue id) (getOption args name)
