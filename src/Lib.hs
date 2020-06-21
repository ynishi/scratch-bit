module Lib where

import           Data.Time.Calendar
import           Data.Time.Clock

type Trans = BlockChain

type Blk = BlockChain

data BlockChain
  = BlockChain { _pool  :: [Trans]
               , _chain :: [Blk] }
  | Transaction
  | Block { _ts           :: UTCTime
          , _transactions :: [Trans]
          , _nonce        :: Integer
          , _prev_hash    :: String }
  deriving (Show)

defaultBlockChain now = BlockChain {_pool = [], _chain = [defaultBlock now]}

defaultBlock now = Block now [] 0 "init hash"

updateChain bc block = bc {_chain = _chain bc ++ [block]}

pp (BlockChain _ c) =
  putStrLn $
  concatMap
    (\(i, xs) ->
       take 25 (repeat '=') ++
       " Chain " ++
       show i ++
       " " ++
       take 25 (repeat '=') ++
       "\n" ++
       (show $ xs {_transactions = []}) ++
       "\ntransactions:" ++
       concatMap (\x -> show x ++ "\n") (_transactions xs) ++ "\n")
    (zip [0 ..] c) ++
  take 20 (repeat '*')

doMain = do
  d1 <- getCurrentTime
  let blockChain1 = defaultBlockChain d1
  pp blockChain1
  d2 <- getCurrentTime
  let blockChain2 = updateChain blockChain1 (Block d2 [] 3 "hash 1")
  pp blockChain2
  d3 <- getCurrentTime
  let blockChain3 = updateChain blockChain2 (Block d3 [] 5 "hash 3")
  pp blockChain3
