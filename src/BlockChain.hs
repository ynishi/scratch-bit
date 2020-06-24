{-# LANGUAGE LambdaCase #-}

module BlockChain where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Utils

type Trans = BlockChain

type Blk = BlockChain

data BlockChain
  = BlockChain { _pool  :: [Trans]
               , _chain :: [Blk] }
  | Transaction { _sender    :: String
                , _recipient :: String
                , _value     :: Float }
  | Block { _ts           :: UTCTime
          , _transactions :: [Trans]
          , _nonce        :: Integer
          , _prev_hash    :: String }
  deriving (Show)

defaultBlockChain now = BlockChain {_pool = [], _chain = [defaultBlock now]}

defaultBlock now = Block now [] 0 (sha "")

updateChain bc now nonce =
  bc
    { _pool = []
    , _chain =
        _chain bc ++
        [ Block
            { _ts = now
            , _nonce = nonce
            , _transactions = _pool bc
            , _prev_hash = hashBlock $ last (_chain bc)
            }
        ]
    }

hashBlock block = sha . show $ block

addTrans bc trans = bc {_pool = _pool bc ++ [trans]}

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
  let blockChain1a = addTrans blockChain1 (Transaction "A" "B" 1.0)
  pp blockChain1a
  let prev_hash1 = hashBlock $ last (_chain blockChain1a)
  d2 <- getCurrentTime
  let blockChain2 = updateChain blockChain1a d2 3
  let blockChain2a = addTrans blockChain2 (Transaction "C" "D" 2.0)
  let blockChain2b = addTrans blockChain2a (Transaction "X" "Y" 3.0)
  pp blockChain2b
  d3 <- getCurrentTime
  let blockChain3 = updateChain blockChain2b d3 5
  pp blockChain3
