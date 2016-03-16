{-# LANGUAGE OverloadedStrings          #-}
-- base64-string is required, one may install via cabal
module ReaderMonad where

import Control.Monad.Reader
import Codec.Binary.Base64.String as B64

cypherHead :: String -> Reader String String
cypherHead header = do
    secret <- ask
    return (B64.encode $ secret ++ header)

cypherBody :: String -> Reader String String
cypherBody payload = do
  secret <- ask
  return (B64.encode $ secret ++ payload)

pseudoJWT :: String -> String -> Reader String String
pseudoJWT plainHeader plainPayload = do
    header <- cypherHead plainHeader
    body <- cypherBody plainPayload
    signature <- createSignature header body
    return (header ++ "." ++ body ++ "." ++ signature)
    where
        createSignature :: String -> String -> Reader String String
        createSignature header body = do
            secret <- ask
            return (B64.encode $ secret ++ header ++ body)
        

exampleJWT :: String
exampleJWT = (runReader $ pseudoJWT "header" "payload") "secretKeyword" 

