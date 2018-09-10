{-# LANGUAGE OverloadedStrings #-}
module Main where

import ClassyPrelude
import qualified Data.Char as Char
import qualified Data.Maybe as M
import qualified Data.Version as Sys
import qualified HeightMap as HM
import qualified Paths_diamond_square as Sys
import qualified Prelude
import qualified System.Console.CmdArgs.Explicit as CMD
import qualified System.Exit as Sys
import qualified Text.Read as Read

main :: IO ()
main = do
    configuration <- parseArguments

    let sideLen = getSideLen configuration
        mapConfiguration = getConfiguration configuration

    heightMap <- HM.createMapWithConfiguration sideLen mapConfiguration

    case HM.saveMap heightMap of
        Left err -> hSayShow stderr err >> Sys.exitWith (Sys.ExitFailure (-1))
        Right bs -> saveBS configuration bs

{- | Parse arguments given to program. Exit on help requests, version requests
 - and if arguments are invalid. Otherwise return the parsed configuration. -}
parseArguments :: MonadIO m => m Configuration
parseArguments = liftIO (CMD.processArgs arguments) >>= getConfigurationOrExit

{- | Configuration used to decide how to run the program. The configuration is
 - created from arguments given to the program. -}
data Configuration
    -- | Write the generated map to stdout.
    = StdoutConfig SideLen (HM.Configuration Roughness)
    -- | Write the generated map to a file with the given name.
    | FileConfig SideLen (HM.Configuration Roughness) FilePath

-- | The side length of a map.
type SideLen = Int

-- | How rough to make the map.
type Roughness = Double

{- | Get the side length from the configuration. -}
getSideLen
    :: Configuration
    -- ^ Configuration to extract side length from.
    -> SideLen
getSideLen (StdoutConfig sideLen _) = sideLen
getSideLen (FileConfig sideLen _ _) = sideLen

{- | Get the map configuration from a argument configuration. -}
getConfiguration
    :: Configuration
    -- ^ Configuration to extract map configuration from.
    -> HM.Configuration Double
getConfiguration (StdoutConfig _ config) = config
getConfiguration (FileConfig _ config _) = config

{- | Save a bytestring to either a file or stdout depending on the
 - configuration given. -}
saveBS
    :: MonadIO m
    => Configuration
    -- ^ Configuration to determine output location.
    -> ByteString
    -- ^ Bytestring to save.
    -> m ()
saveBS (FileConfig _ _ outfile) = writeFile outfile
saveBS (StdoutConfig _ _) = hPut stdout

{- | Configuration properties parsed from the command line given. These
 - properties should be transformed into an actual configuration via
 - getConfigurationOrExit and at the same time non valid configurations
 - should be rejected. -}
data Arguments = Arguments
    { _sideLength :: Maybe SideLen -- ^ The side length of the map to generate.
    , _outfile    :: Maybe FilePath -- ^ Where to write the map.
    , _roughness  :: Maybe Roughness -- ^ How rough the map should be.
    , _help       :: Bool -- ^ Whether or not to display help message.
    , _version    :: Bool -- ^ Whether or not to display program version.
    } deriving (Show, Eq)

{- | Define command line parser for the program. -}
arguments :: CMD.Mode Arguments
arguments = CMD.mode "square-diamond" defaultConfig helpMsg argParse flags
  where
    defaultConfig = Arguments Nothing Nothing Nothing False False

    argParse = CMD.flagArg updateSideLen "INT"
    updateSideLen newSideLen config
        | _sideLength config /= Nothing = Left "Only one argument expected."
        | (not . all Char.isNumber) newSideLen = Left "Could not parse number."
        | otherwise = Right $ config { _sideLength = Just $ Prelude.read newSideLen }

    flags = [ outfileFlag, roughFlag, helpFlag, versionFlag ]

    outfileFlag = CMD.flagReq [ "outfile", "f" ] updateOutfile "FILE"
        "Where to write output."
    updateOutfile newOutfile config
        | _outfile config /= Nothing = Left "Only one output file expected."
        | otherwise = Right $ config { _outfile = Just newOutfile }

    roughFlag = CMD.flagReq [ "roughness", "r" ] updateRoughness "REAL"
        "Between 0.0 and 1.0. The closer to 1.0 the rougher the map."
    updateRoughness newRoughness config
        | _roughness config /= Nothing = Left "Only one roughness expected."
        | (Read.readMaybe newRoughness :: Maybe Double) == Nothing =
            Left "Roughness should be a number"
        | (Read.read newRoughness :: Double) < 0.0 ||
            (Read.read newRoughness :: Double) > 1.0 =
            Left "Roughness should be between 0.0 and 1.0."
        | otherwise = Right $ config { _roughness = Just $ Read.read newRoughness }

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
getConfigurationOrExit
    :: MonadIO m
    => Arguments
    -- ^ Arguments parsed from the command line.
    -> m Configuration
getConfigurationOrExit (Arguments _ _ _ True _) = do
    hSayShow stderr $ CMD.helpText [] CMD.HelpFormatAll arguments
    liftIO $ Sys.exitWith Sys.ExitSuccess
getConfigurationOrExit (Arguments _ _ _ _ True) = do
    hSay stderr $ concat
        [ "diamond-square v"
        , pack $ Sys.showVersion Sys.version
        , " (C) Magnus Stavngaard"
        ]
    liftIO $ Sys.exitWith Sys.ExitSuccess
getConfigurationOrExit (Arguments (Just sideLen) Nothing roughness _ _) =
    case HM.createConfiguration (M.fromMaybe 1.0 roughness) of
        Left err -> do
            hSay stderr err
            liftIO $ Sys.exitWith (Sys.ExitFailure (-1))
        Right config -> pure $ StdoutConfig sideLen config
getConfigurationOrExit (Arguments (Just sideLen) (Just file) roughness _ _) =
    case HM.createConfiguration (M.fromMaybe 1.0 roughness) of
        Left err -> do
            hSay stderr err
            liftIO $ Sys.exitWith (Sys.ExitFailure (-1))
        Right config -> pure $ FileConfig sideLen config file
getConfigurationOrExit _ = do
    hSayShow stderr $ CMD.helpText [] CMD.HelpFormatAll arguments
    liftIO $ Sys.exitWith (Sys.ExitFailure (-1))
