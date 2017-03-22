{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((.~), (&))
import Data.UnixTime(addUnixDiffTime, secondsToUnixDiffTime)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import CommandLineParser
    ( parseCommandLine
    , ConnectMode(..)
    , CreatedOption(..)
    , ExportOptions(..)
    )
import Paths_stripe_export (version)
import Pipes (runEffect, (>->))
import StripeExport.Pipes
    ( requestAll
    , requestAllConnect
    , encodeJSON
    , stdoutJSONL
    )
import StripeExport.Raw
    ( defaultRequestParameters
    , createdGTE
    , createdLT
    , limit
    , RequestParameters
    )
import System.Environment (getEnv)

main = do
    commandLineOptions <- parseCommandLine (showVersion version) $(gitHash)
    stripeApiSecret <- getEnv "STRIPE_API_SECRET"
    let parameters = requestParameters stripeApiSecret commandLineOptions
    runEffect $ requestAll parameters >-> encodeJSON >-> stdoutJSONL
    case connectMode commandLineOptions of
        ExcludeConnect -> return ()
        IncludeConnect -> runEffect $ requestAllConnect parameters >-> encodeJSON >-> stdoutJSONL
    where
        requestParameters secret opts = defaultRequestParameters secret (endpointPath opts)
            & limit .~ Just 100
            & createdSetter (createdOption opts)

        addDay date = addUnixDiffTime date (secondsToUnixDiffTime 24*60*60)

        createdSetter :: Maybe CreatedOption -> RequestParameters -> RequestParameters
        createdSetter Nothing rps = rps
        createdSetter (Just (CreatedDate date)) rps =
            rps & createdGTE .~ Just date & createdLT .~ (Just $ addDay date)
        createdSetter (Just (CreatedRange gte lt)) rps =
            rps & createdGTE .~ Just gte & createdLT .~ Just lt
