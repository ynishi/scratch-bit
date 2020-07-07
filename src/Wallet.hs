{-# LANGUAGE PackageImports #-}

module Wallet where

import           Crypto.Number.Serialize
import qualified Crypto.PubKey.ECC.Generate    as GEN
import qualified "crypto-random" Crypto.Random as RND
import qualified Crypto.Types.PubKey.ECC       as ECC
import qualified Crypto.Types.PubKey.ECDSA     as ECDSA
import           "crypto-api" Crypto.Util      (bs2i, i2bs, i2bs_unsized)
import qualified Data.ByteString               as BS
import           Utils

data Wallet = Wallet
  { _private_key :: ECDSA.PrivateKey
  , _public_key  :: ECDSA.PublicKey
  }

createWallet = do
  (pub, pri) <- genKey
  return $ Wallet {_private_key = pri, _public_key = pub}

genKey = do
  ep <- RND.createEntropyPool
  let cprg = RND.cprgCreate ep :: RND.SystemRNG
  let (kp, _) = GEN.generate cprg (ECC.getCurveByName ECC.SEC_p192r1)
  return kp

showPrivate wallet = fromKeyInt $ ECDSA.private_d $ _private_key wallet

showPublic wallet = hex $ publicToBytes $ _public_key wallet

publicToBytes :: ECDSA.PublicKey -> BS.ByteString
publicToBytes (ECDSA.PublicKey (ECC.CurveFP (ECC.CurvePrime _ ECC.CurveCommon {ECC.ecc_n = n})) (ECC.Point x y)) =
  BS.singleton
    (if y `mod` 2 == 0
       then 0x02
       else 0x03) `BS.append`
  i2bs (8 * lengthBytes n) x
