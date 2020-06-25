{-# LANGUAGE LambdaCase #-}

module BlockChain where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Debug.Trace
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

updateChain bc now =
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
  where
    nonce = proofOfWork bc now

hashBlock = sha . show

difficulty = 2

solvNonce :: BlockChain -> Bool -> Integer
solvNonce block True = _nonce block - 1
solvNonce block False =
  solvNonce
    block {_nonce = _nonce block + 1}
    (all (uncurry (==)) $ zip (take difficulty (hashBlock block)) (repeat '0'))

proofOfWork bc now = solvNonce block False
  where
    trs = _pool bc
    prev = last (_chain bc)
    prev_hash = hashBlock prev
    nonce = 0
    block =
      Block
        { _ts = _ts prev
        , _transactions = trs
        , _nonce = nonce
        , _prev_hash = prev_hash
        }

addTrans bc trans = bc {_pool = _pool bc ++ [trans]}

pp (BlockChain _ c) =
  putStrLn $
  concatMap
    (\(i, xs) ->
       replicate 25 '=' ++
       " Chain " ++
       show i ++
       " " ++
       replicate 25 '=' ++
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
  replicate 20 '*'

doMain = do
  d1 <- getCurrentTime
  let blockChain1 = defaultBlockChain d1
  let blockChain1a = addTrans blockChain1 (Transaction "A" "B" 1.0)
  pp blockChain1a
  d2 <- getCurrentTime
  let blockChain2 = updateChain blockChain1a d2
  let blockChain2a = addTrans blockChain2 (Transaction "C" "D" 2.0)
  let blockChain2b = addTrans blockChain2a (Transaction "X" "Y" 3.0)
  pp blockChain2b
  d3 <- getCurrentTime
  let blockChain3 = updateChain blockChain2b d3
  pp blockChain3
