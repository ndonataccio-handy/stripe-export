module CommandLineParser
    ( ConnectMode(..)
    , CreatedOption(..)
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
    , Parser
    , ParserInfo
    , progDesc
    , short
    , strArgument
    )

data CreatedOption
    = CreatedDate UnixTime
    | CreatedRange UnixTime UnixTime

data ConnectMode = IncludeConnect | ExcludeConnect deriving Show

data ExportOptions = ExportOptions
    { endpointPath :: String
    , connectMode :: ConnectMode
    , createdOption :: Maybe CreatedOption
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

exportOptions :: Parser ExportOptions
exportOptions = ExportOptions
    <$> endpointPathOptionParser
    <*> connectModeOptionParser
    <*> createdOptionParser

endpointPathOptionParser :: Parser String
endpointPathOptionParser = strArgument $
       metavar "ENDPOINT_PATH"
    <> help "Endpoint path, such as `events' or `balance/history'"

connectModeOptionParser = excludeConnectFlag <|> includeConnectFlag
    where
        excludeConnectFlag = flag' ExcludeConnect $
               long "exclude-connect"
            <> help "Exclude data scoped to connect accounts"

        includeConnectFlag = flag ExcludeConnect IncludeConnect $
               long "include-connect"
            <> short 'c'
            <> help "Include data scoped to connect accounts"

createdOptionParser :: Parser (Maybe CreatedOption)
createdOptionParser = optional
      $ CreatedDate <$> createdDateOptionParser
    <|> CreatedRange <$> createdGTEOptionParser <*> createdLTOptionParser
    where
        createdDateOptionParser :: Parser UnixTime
        createdDateOptionParser = option dateReader $
               long "created-date"
            <> metavar "YYYY-MM-DDTHH:MM:SS"
            <> help "Restrict export to records created on <created-date> (UTC)"

        createdGTEOptionParser :: Parser UnixTime
        createdGTEOptionParser = option timestampReader $
               long "created-gte"
            <> metavar "YYYY-MM-DDTHH:MM:SS"
            <> help "Restrict export to records created after or equal to <created-gte> (UTC)"

        createdLTOptionParser :: Parser UnixTime
        createdLTOptionParser = option timestampReader $
               long "created-lt"
            <> metavar "YYYY-MM-DDTHH:MM:SS"
            <> help "Restrict export to records created before <created-lt> (UTC)"

        timestampReader = eitherReader $
            Right . parseUnixTimeGMT "%Y-%m-%dT%H:%M:%S" . pack

        dateReader = eitherReader $
            Right . parseUnixTimeGMT "%Y-%m-%d" . pack
