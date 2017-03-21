{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((.~), (&))
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import CommandLineParser (parseCommandLine, ConnectMode(..), ExportOptions(..))
import Paths_stripe_export (version)
import Pipes (runEffect, (>->))
import StripeExport.Pipes (requestAll, requestAllConnect, encodeJSON, stdoutJSONL)
import StripeExport.Raw
    ( defaultRequestParameters
    , createdGTE
    , createdLT
    , limit
    )
import System.Environment (getEnv)

main = do
    commandLineOptions <- parseCommandLine (showVersion version) $(gitHash)
    stripeApiSecret <- getEnv "STRIPE_API_SECRET"
    let parameters = requestParameters stripeApiSecret commandLineOptions
    runEffect $ requestAll parameters >-> encodeJSON >-> stdoutJSONL
    case _connectMode commandLineOptions of
        ExcludeConnect -> return ()
        IncludeConnect -> runEffect $ requestAllConnect parameters >-> encodeJSON >-> stdoutJSONL
    where
        requestParameters secret opts = defaultRequestParameters secret (_endpointPath opts)
            & createdGTE .~ _createdGTE opts
            & createdLT .~ _createdLT opts
            & limit .~ Just 100
