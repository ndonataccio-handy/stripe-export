module StripeExport.Pipes
    ( requestAll
    , requestAllConnect
    , encodeJSON
    , stdoutJSONL
    )where

import Control.Lens ((^.), (^..), (^?), (&), (.~), (?~), lastOf)
import Control.Lens.At (at)
import Data.Aeson (encode, Value(..))
import Data.Aeson.Lens (_Bool, key, _Object, _String, values)
import qualified Data.ByteString.Lazy.Char8 as B (ByteString, putStrLn)
import Data.Maybe (isNothing)
import Data.Text (pack, unpack)
import Control.Monad (unless)
import Pipes
import StripeExport.Raw
    ( defaultRequestParameters
    , RequestParameters
    , requestRaw
    , apiSecret
    , connectAccountId
    , startingAfter
    )

requestAll :: RequestParameters -> Producer Value IO ()
requestAll parameters = do
    bytes <- lift $ requestRaw parameters
    let items = bytes ^.. key "data" . values
    let hasMore = foldr const False (bytes ^? key "has_more" . _Bool)
    let maybeLastId = fmap unpack $ lastOf traverse items >>= (^? key "id" . _String)
    each items
    unless (isNothing maybeLastId || not hasMore)
           (requestAll $ parameters & startingAfter .~ maybeLastId)

-- This requests all the objects scoped to any connect account. We have to iterate over each
-- connect account and make a requestAll call to accumulate all of the objects.
requestAllConnect :: RequestParameters -> Producer Value IO ()
requestAllConnect parameters =
    for (requestAll connectParameters) requestValues
    where
        connectParameters = defaultRequestParameters (parameters ^. apiSecret) "accounts"
        requestValues account = for
            (requestAll $ parameters & connectAccountId .~ Just connectId)
            (yield . annotateWithConnectId)
            where
                connectId = unpack $ foldr const (error "Missing id") maybeId
                maybeId = account ^? key "id" . _String
                annotateWithConnectId object =
                  Object $ object ^. _Object & at "connect_id" ?~ String (pack connectId)

stdoutJSONL :: Consumer B.ByteString IO ()
stdoutJSONL = for cat (lift . B.putStrLn)

encodeJSON :: Pipe Value B.ByteString IO ()
encodeJSON = for cat (yield . encode)
