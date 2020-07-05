module Wallet where

import qualified Crypto.PubKey.ECC.Generate as GEN
import qualified Crypto.Random              as RND
import qualified Crypto.Types.PubKey.ECC    as ECC
import qualified Crypto.Types.PubKey.ECDSA  as ECDSA
import           Utils

-- import qualified Crypto.PubKey.ECDSA as ECDSA
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

-- TODO: show as hex
showPrivate wallet = show $ _private_key wallet

showPublic wallet = show $ _public_key wallet
