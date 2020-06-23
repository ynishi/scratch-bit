{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Crypto.Hash.SHA256    as SHA256
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import           Numeric               (showHex)

hex = concatMap (flip showHex "") . B.unpack

sha s = hex digest
  where
    digest = SHA256.finalize ctx
    ctx = SHA256.update ctx0 $ C8.pack "test"
    ctx0 = SHA256.init
