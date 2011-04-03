{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module RestServer (runServer) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Text.Regex.TDFA
import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8 (fromShow, fromChar)
import Data.Monoid
import Data.List (intersperse)

import Trie

--------------------------  Text mangling ----------------------------

-- | Get the value of a query parameter.
getOption :: ByteString -> ByteString -> Maybe ByteString
getOption param qs = case groups of
                       []  -> Nothing
                       x:_ -> Just x
    where (_::ByteString, _::ByteString, _::ByteString, groups) = 
              qs =~ (B.concat ["[?&]", param, "=([^?&]*)"])

-- | Convert completion list to JSON
jsonList :: [(ByteString, Int)] -> Builder
jsonList l = c "[" `mappend` (mconcat $ intersperse (c ",") $ map showPair l) `mappend` c "]"
    where showPair (s, x) = mconcat [c "[\"", escape s, c "\",", fromShow x, c "]"]
          c = copyByteString
          escape :: ByteString -> Builder
          escape = mconcat . (map escapeChar) . B.unpack
          escapeChar '"' = c "\\\""
          escapeChar '\\' = c "\\\\"
          escapeChar '\b' = c "\\b"
          escapeChar '\f' = c "\\f"
          escapeChar '\n' = c "\\n"
          escapeChar '\r' = c "\\r"
          escapeChar '\t' = c "\\t"
          escapeChar '\0' = c "\\0"
          escapeChar char = fromChar char

-- | Convert completion list to JSONP
jsonPList :: ByteString -> [(ByteString, Int)] -> Builder
jsonPList callback l = c callback `mappend` fromChar '(' `mappend` jsonList l `mappend` c ");"
    where c = copyByteString

---------------------------- HTTP serving ----------------------------

app :: Application
app req = if "/complete/" `B.isPrefixOf` (pathInfo req)
            then lookupApp req
            else return error404

error404 :: Response
error404 = ResponseBuilder status404 [("Content-Type", "text/html")]
             (copyByteString "<h1>404 Not Found</h1>\n")

lookupApp :: Application
lookupApp req = do completions <- liftIO $ autocomplete prefix
                   return $ complete completions callback
    where prefix = B.drop 10 (pathInfo req)
          callback = getOption "callback" (queryString req)

complete :: [(ByteString, Int)] -> Maybe ByteString -> Response
complete completions Nothing = ResponseBuilder status200 headers body
    where headers = [("Content-Type", "application/json"),
                     ("Cache-Control", "max-age=60")]
          body = jsonList completions

complete completions (Just callback) = ResponseBuilder status200 headers body
    where headers = [("Content-Type", "text/javascript"),
                     ("Cache-Control", "max-age=60")]
          body = jsonPList callback completions

runServer :: Int -> IO ()
runServer port = run port app
