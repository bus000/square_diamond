{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import ClassyPrelude
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Version as Sys
import qualified HeigtMapMutVec as HM
import qualified Paths_diamond_square as Sys
import qualified Prelude as Prelude
import qualified System.Console.CmdArgs.Explicit as CMD
import qualified System.Environment as Sys
import qualified System.Exit as Sys

main :: IO ()
main = do
    configuration <- parseArguments

    let sideLen = getSideLen configuration

    heightMap <- HM.createMap sideLen

    case configuration of
        StdoutConfig _ -> HM.saveMap heightMap "stdout"
        FileConfig _ outfile -> HM.saveMap heightMap outfile

{- | Parse arguments given to program. Exit on help requests, version requests
 - and if arguments are invalid. Otherwise return the parsed configuration. -}
parseArguments :: IO Configuration
parseArguments = CMD.processArgs arguments >>= getConfigurationOrExit

{- | Configuration used to decide how to run the program. The configuration is
 - created from arguments given to the program. -}
data Configuration
    -- | Write the generated map to stdout.
    = StdoutConfig SideLen
    -- | Write the generated map to a file with the given name.
    | FileConfig SideLen FilePath
  deriving (Show)

-- | The side length of a map.
type SideLen = Int

{- | Get the side length from the configuration. -}
getSideLen :: Configuration
    -- ^ Configuration to extract side length from.
    -> SideLen
getSideLen (StdoutConfig sideLen) = sideLen
getSideLen (FileConfig sideLen _) = sideLen

{- | Configuration properties parsed from the command line given. These
 - properties should be transformed into an actual configuration via
 - getConfigurationOrExit and at the same time non valid configurations
 - should be rejected. -}
data Arguments = Arguments
    { _sideLength :: Maybe SideLen -- ^ The side length of the map to generate.
    , _outfile    :: Maybe FilePath -- ^ Where to write the map.
    , _help       :: Bool -- ^ Whether or not to display help message.
    , _version    :: Bool -- ^ Whether or not to display program version.
    } deriving (Show, Eq)

{- | Define command line parser for the program. -}
arguments :: CMD.Mode Arguments
arguments = CMD.mode "square-diamond" defaultConfig helpMsg argParse flags
  where
    defaultConfig = Arguments Nothing Nothing False False

    argParse = CMD.flagArg updateSideLen "INT"
    updateSideLen newSideLen config
        | _sideLength config /= Nothing = Left "Only one argument expected."
        | (not . all Char.isNumber) newSideLen = Left "Could not parse number."
        | otherwise = Right $ config { _sideLength = Just $ Prelude.read newSideLen }

    flags = [ outfileFlag, helpFlag, versionFlag ]

    outfileFlag = CMD.flagReq [ "outfile", "f" ] updateOutfile "FILE"
        "Where to write output."
    updateOutfile newOutfile config
        | _outfile config /= Nothing = Left "Only one output file expected."
        | otherwise = Right $ config { _outfile = Just newOutfile }

    helpFlag = CMD.flagNone [ "help", "h" ] updateHelp "Display this message."
    updateHelp config = config { _help = True }

    versionFlag = CMD.flagNone [ "version", "v" ] updateVersion
        "Display version information."
    updateVersion config = config { _version = True }

    helpMsg =
        "Generate random height maps using the square diamond algorithm. A square\
        \ map will be generated and saved as a PNG file. If outfile is specified\
        \ then the PNG file is saved to that location, otherwise it is written to\
        \ stdout."

{- | Get configuration from the parsed arguments or exit the program if needed.
 - The cases where the program is exited is,
 -
 - 1. _help is true (print help message and exit with ExitSuccess),
 - 2. _version is true (print version and exit with ExitSuccess),
 - 3. arguments are invalid (print help message and exit with ExitFailure). -}
getConfigurationOrExit :: MonadIO m => Arguments
    -- ^ Arguments parsed from the command line.
    -> m Configuration
getConfigurationOrExit (Arguments _ _ True _) = do
    hSayShow stderr $ CMD.helpText [] CMD.HelpFormatAll arguments
    liftIO $ Sys.exitWith Sys.ExitSuccess
getConfigurationOrExit (Arguments _ _ _ True) = do
    hSay stderr $ T.concat
        [ "diamond-square v"
        , T.pack $ Sys.showVersion Sys.version
        , " (C) Magnus Stavngaard"
        ]
    liftIO $ Sys.exitWith Sys.ExitSuccess
getConfigurationOrExit (Arguments (Just sideLen) Nothing _ _) =
    pure $ StdoutConfig sideLen
getConfigurationOrExit (Arguments (Just sideLen) (Just file) _ _) =
    pure $ FileConfig sideLen file
getConfigurationOrExit _ = do
    hSayShow stderr $ CMD.helpText [] CMD.HelpFormatAll arguments
    liftIO $ Sys.exitWith (Sys.ExitFailure (-1))
