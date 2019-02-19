{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Parse
    ( -- * Parsing whole requests and responses
      request, Request(..)
    , response, Response(..)
    ) where

import Control.Monad (void)
import Control.Applicative (many)

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import qualified Network.HTTP.Types as H
import Data.CaseInsensitive as CI

import Prelude hiding (takeWhile)

data Request = Request { reqMethod  :: H.StdMethod
                       , reqUri     :: BS.ByteString
                       , reqVersion :: H.HttpVersion
                       , reqHeaders :: H.RequestHeaders
                       }
             deriving (Show)

data Response = Response { respStatus  :: H.Status
                         , respVersion :: H.HttpVersion
                         , respHeaders :: H.ResponseHeaders
                         }
              deriving (Show)

headers :: Parser [H.Header]
headers = many header
  where
    header = do
        name <- takeWhile1 (notInClass ":\n")
        char ':'
        skipSpace
        value <- takeWhile1 (/= '\n')
        newLine
        return (CI.mk name, value)

method :: Parser H.StdMethod
method = do
    s <- takeWhile isAlpha_ascii
    either (const $ fail $ "invalid method "++show s) pure $ H.parseMethod s

version :: Parser H.HttpVersion
version = do
    void $ string "HTTP/"
    maj <- decimal
    void $ char '.'
    min <- decimal
    return $ H.HttpVersion maj min

newLine :: Parser ()
newLine = void $ char '\n'

request :: Parser Request
request = do
    reqMethod <- method
    void space
    reqUri <- takeTill isSpace
    void space
    reqVersion <- version
    newLine
    reqHeaders <- headers
    newLine
    return $ Request {..}

response :: Parser Response
response = do
    respVersion <- version
    void space
    statusCode <- decimal
    void space
    statusMessage <- takeWhile1 (/= '\n')
    newLine
    let respStatus = H.Status {..}
    respHeaders <- headers
    return $ Response {..}
