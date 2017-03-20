module StripeExport
    ( defaultRequestParameters
    , exportRaw
    , RequestParameters(..)
    ) where

import Codec.Binary.Base64.String (encode)
import Control.Exception (catch, throwIO)
import Control.Lens ((&), (.~), view)
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
    { apiSecret :: String
    , endpointPath :: String
    , connectAccountId :: Maybe String
    , createdGTE :: Maybe UnixTime
    , createdLT :: Maybe UnixTime
    , limit :: Maybe Int
    } deriving Show

defaultRequestParameters :: String -> String -> RequestParameters
defaultRequestParameters secret path = RequestParameters
    { apiSecret = secret
    , endpointPath = path
    , connectAccountId = Nothing
    , createdGTE = Nothing
    , createdLT = Nothing
    , limit = Nothing
    }

parametersSetter RequestParameters{..} =
      (header "Authorization" .~ ["Basic " <> pack (encode apiSecret)])
    . maybe param "created[gte]" (encodeUnixTime <$> createdGTE)
    . maybe param "created[lt]" (encodeUnixTime <$> createdLT)
    . maybe param "limit" (T.pack . show <$> limit)
    . maybe header "Stripe-Account" (pack <$> connectAccountId)
    where
        encodeUnixTime = T.pack . show . fromEnum . toEpochTime
        maybe f k = foldr (\v _ -> f k .~ [v]) id

exportRaw :: RequestParameters -> IO ByteString
exportRaw parameters =
    view responseBody <$> getWith options fullPath `catch` httpErrorHandler

    where
        fullPath = "https://api.stripe.com/v1/" <> endpointPath parameters
        timeout = responseTimeoutMicro 600000000
        options = defaults
            & parametersSetter parameters
            & manager .~ Left (tlsManagerSettings { managerResponseTimeout = timeout })

        -- Avoid printing out the api secret
        httpErrorHandler (HttpExceptionRequest _ c) = die $ "stripe-export: " <> show c
        httpErrorHandler e = throwIO e
