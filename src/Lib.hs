{-# LANGUAGE LambdaCase #-}

module Lib where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Utils

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

defaultBlock now = Block now [] 0 (sha "")

updateChain bc block = bc {_chain = _chain bc ++ [block]}

hashBlock block = sha . show $ block

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
       map
         (\case
            '[' -> '\n'
            '{' -> '\n'
            ',' -> '\n'
            '}' -> '\n'
            ']' -> '\n'
            c -> c)
         (show xs))
    (zip [0 ..] c) ++
  take 20 (repeat '*')

doMain = do
  putStrLn $ sha "test"
  d1 <- getCurrentTime
  let blockChain1 = defaultBlockChain d1
  pp blockChain1
  let prev_hash1 = hashBlock $ last (_chain blockChain1)
  d2 <- getCurrentTime
  let blockChain2 = updateChain blockChain1 (Block d2 [] 3 prev_hash1)
  let prev_hash2 = hashBlock $ last (_chain blockChain2)
  pp blockChain2
  d3 <- getCurrentTime
  let blockChain3 = updateChain blockChain2 (Block d3 [] 5 prev_hash2)
  pp blockChain3
