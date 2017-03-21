{-# LANGUAGE TemplateHaskell #-}

module StripeExport.Raw
    ( requestRaw
    , RequestParameters
    , defaultRequestParameters
    , apiSecret
    , endpointPath
    , connectAccountId
    , createdGTE
    , createdLT
    , limit
    , startingAfter
    ) where

import Codec.Binary.Base64.String (encode)
import Control.Exception (catch, throwIO)
import Control.Lens ((^.), (&), (.~), view)
import Control.Lens.TH (makeLenses)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Data.Semigroup((<>))
import qualified Data.Text as T (pack)
import Data.UnixTime(toEpochTime, UnixTime)
import Network.Wreq (defaults, header, getWith, manager, param, responseBody)
import Network.HTTP.Client (HttpException(..), managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit(die)

data RequestParameters = RequestParameters
    { _apiSecret :: String
    , _endpointPath :: String
    , _connectAccountId :: Maybe String
    , _createdGTE :: Maybe UnixTime
    , _createdLT :: Maybe UnixTime
    , _limit :: Maybe Int
    , _startingAfter :: Maybe String
    } deriving Show

makeLenses ''RequestParameters

defaultRequestParameters :: String -> String -> RequestParameters
defaultRequestParameters secret path = RequestParameters
    { _apiSecret = secret
    , _endpointPath = path
    , _connectAccountId = Nothing
    , _createdGTE = Nothing
    , _createdLT = Nothing
    , _limit = Nothing
    , _startingAfter = Nothing
    }

parametersSetter RequestParameters{..} =
      (header "Authorization" .~ ["Basic " <> pack (encode _apiSecret)])
    . maybe param "created[gte]" (encodeUnixTime <$> _createdGTE)
    . maybe param "created[lt]" (encodeUnixTime <$> _createdLT)
    . maybe param "limit" (T.pack . show <$> _limit)
    . maybe param "starting_after" (T.pack<$> _startingAfter)
    . maybe header "Stripe-Account" (pack <$> _connectAccountId)
    where
        encodeUnixTime = T.pack . show . fromEnum . toEpochTime
        maybe f k = foldr (\v _ -> f k .~ [v]) id

requestRaw :: RequestParameters -> IO ByteString
requestRaw parameters =
    view responseBody <$> getWith options fullPath `catch` httpErrorHandler

    where
        fullPath = "https://api.stripe.com/v1/" <> parameters ^. endpointPath
        timeout = responseTimeoutMicro 600000000
        options = defaults
            & parametersSetter parameters
            & manager .~ Left (tlsManagerSettings { managerResponseTimeout = timeout })

        -- Avoid printing out the api secret
        httpErrorHandler (HttpExceptionRequest _ c) = die $ "stripe-export: " <> show c
        httpErrorHandler e = throwIO e
