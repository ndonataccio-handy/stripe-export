module CommandLineParser
    ( ConnectMode(..)
    , ExportOptions(..)
    , parseCommandLine
    ) where

import Data.ByteString.Char8 (pack)
import Data.Semigroup ((<>))
import Data.UnixTime (parseUnixTimeGMT, UnixTime)
import Options.Applicative
    ( (<*>)
    , (<|>)
    , eitherReader
    , execParser
    , flag
    , flag'
    , fullDesc
    , header
    , help
    , helper
    , info
    , infoOption
    , long
    , metavar
    , option
    , optional
    , ParserInfo
    , progDesc
    , short
    , strArgument
    )

data ConnectMode = IncludeConnect | ExcludeConnect deriving Show

data ExportOptions = ExportOptions
    { _endpointPath :: String
    , _createdGTE :: Maybe UnixTime
    , _createdLT :: Maybe UnixTime
    , _connectMode :: ConnectMode
    }

parseCommandLine version commitHash = execParser $ optionsParser version commitHash

optionsParser :: String -> String -> ParserInfo ExportOptions
optionsParser versionString commitHash = info options description
    where
        description = fullDesc
            <> progDesc "stripe-export - export stripe data"
            <> header "stripe-export"
        options = helper <*> versionOption <*> exportOptions
        versionOption = infoOption
            (versionString <> " " <> commitHash)
            (long "version" <> short 'v' <> help "Show version")

exportOptions = ExportOptions
    <$> endpointPathOption
    <*> createdGTEOption
    <*> createdLTOption
    <*> connectModeOption

endpointPathOption = strArgument $
       metavar "ENDPOINT_PATH"
    <> help "Endpoint path, such as `events' or `balance/history'"

unixTimeOption = eitherReader $
    Right . parseUnixTimeGMT "%Y-%m-%dT%H:%M:%S" . pack

createdGTEOption = optional $ option unixTimeOption $
       long "created-gte"
    <> metavar "YYYY-MM-DDTHH:MM:SS"
    <> help "Restrict export to records created after or equal to <created-gte>"

createdLTOption = optional $ option unixTimeOption $
       long "created-lt"
    <> metavar "YYYY-MM-DDTHH:MM:SS"
    <> help "Restrict export to records created before <created-lt>"

connectModeOption = excludeConnectFlag <|> includeConnectFlag
    where
        excludeConnectFlag = flag' ExcludeConnect $
               long "exclude-connect"
            <> help "Exclude data scoped to connect accounts"

        includeConnectFlag = flag ExcludeConnect IncludeConnect $
               long "include-connect"
            <> short 'c'
            <> help "Include data scoped to connect accounts"
