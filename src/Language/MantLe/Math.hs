module Language.MantLe.Math where

import Data.ByteString.UTF8 qualified as BU
import Network.HTTP.Req

-- | Evaluate expression by sending a POST request
evaluate :: String -> IO String
evaluate expr = runReq defaultHttpConfig $ do
  let
    url =
      http "www.wolframcloud.com"
        /: "obj"
        /: "erchiusjzy"
        /: "plain"
    formData = "x" =: expr
    options =
      header
        "EmbedCode-User-Agent"
        "EmbedCode-JavaScript/1.0"
  res <-
    req
      POST
      url
      (ReqBodyUrlEnc formData)
      bsResponse
      $ options
        <> header
          "Content-Type"
          "application/x-www-form-urlencoded"
  return $ BU.toString (responseBody res)
