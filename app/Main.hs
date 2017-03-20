{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Lazy as B (putStr)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import CommandLineParser (parseCommandLine, ExportOptions(..))
import Paths_stripe_export (version)
import StripeExport (exportRaw, RequestParameters(..))
import System.Environment (getEnv)

main = do
    commandLineOptions <- parseCommandLine (showVersion version) $(gitHash)
    stripeApiSecret <- getEnv "STRIPE_API_SECRET"
    rawData <- exportRaw $ requestParamters stripeApiSecret commandLineOptions
    B.putStr rawData
    where
        requestParamters secret opts = RequestParameters
            { apiSecret = secret
            , endpointPath = _endpointPath opts
            , connectAccountId = Nothing
            , createdGTE = _createdGTE opts
            , createdLT = _createdLT opts
            , limit = Just 28
            }
