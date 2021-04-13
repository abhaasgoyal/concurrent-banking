-- 1. Use cabal build
-- 2. Run as cabal run Concurrent -- +RTS -Nx , where x is the number of processors
-- 3. Enjoy lmao
module Transfer
  ( main1
  )
where

import           Control.Monad                  ( when )
import           Control.Concurrent.STM
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import System.Random

-- | Data structure construction
type Name = String
type AccNo = Int
type Balance = Int
data Customer = Customer Name AccNo Balance
type Account = TVar Customer

-- | Show current account balance
showAccountBalance :: Account -> IO Int
showAccountBalance acc = do
  (Customer _ _ balance) <- readTVarIO acc
  print balance
  return balance

-- | Update balances
updateBal :: Account -> Balance -> STM()
updateBal acc newBal = do
  (Customer name accno _) <- readTVar acc
  writeTVar acc (Customer name accno newBal)

-- | Transfer from one account to another if you have a positive sum of money
transfer :: Account -> Account -> Balance -> STM()
transfer from to amount = do
  (Customer _ _ curBalFrom) <- readTVar from
  (Customer _ _ curBalTo) <- readTVar to
  -- Proves that curBalFrom will never goto 0 in the first place
  if curBalFrom < amount
    then retry
    else do
    updateBal from (curBalFrom - amount)
    updateBal to (curBalTo + amount)

-- | When number of transactions are < 100 then send to a different account
changeState :: Int -> TVar Int -> Account -> Account -> STM ()
changeState moneyCount i acc otherAcc = do
  noOfTrans <- readTVar i
  when (noOfTrans < 100) $ do
    transfer acc otherAcc moneyCount
    writeTVar i (noOfTrans + 1)

-- | Customer Controller thread
customer :: TVar Int -> [Account] -> Account -> IO()
customer i accList acc = do
  noOfTrans <- readTVarIO i
  seed <- newStdGen
  (Customer _ accno _) <- readTVarIO acc
  let (randomDelayTime, _) = randomR (0, 100000) seed :: (Int, StdGen)
  threadDelay randomDelayTime
  when (noOfTrans < 100) $ do
    let (moneyCount, _) = randomR (10,50) seed :: (Int, StdGen)
    let otherAccNo = head $ dropWhile (== accno) (randomRs (0,9) seed)
    let otherAcc = accList !! otherAccNo
    atomically $ changeState moneyCount i acc otherAcc
    customer i accList acc

-- | Main thread
main1 = do
  -- No of transaction counter
  noOfTrans <- newTVarIO 0
  -- 10 people account creation
  acc1 <- newTVarIO (Customer "Amy" 0 200)
  acc2 <- newTVarIO (Customer "Beth" 1 200)
  acc3 <- newTVarIO (Customer "Candis" 2 200)
  acc4 <- newTVarIO (Customer "David" 3 200)
  acc5 <- newTVarIO (Customer "Eugene" 4 200)
  acc6 <- newTVarIO (Customer "Frank" 5 200)
  acc7 <- newTVarIO (Customer "Gary" 6 200)
  acc8 <- newTVarIO (Customer "Hank" 7 200)
  acc9 <- newTVarIO (Customer "Ivy" 8 200)
  acc10 <- newTVarIO (Customer "Joker" 9 200)
  let accList = [acc1, acc2, acc3, acc4, acc5, acc6, acc7, acc8, acc9, acc10]
  putStrLn "Starting.."
  -- mapConcurrently maps an IO action in parallel and asynchronously to the elements of Traversable classes such as Lists.. Automatically takes care when the cores are increased... It is part of the async library and waits till all actions are done
  -- Control.Parallel could have been needed to speedup when Monadics weren't involved (in this case the IO Monad)
  -- So there is no need for Control.Parallel when such a nice function is already there yay
  mapConcurrently_ (customer noOfTrans accList) accList
  putStrLn "Done!"
  putStrLn "Final Balances: "
  balances <- mapM showAccountBalance accList
  if sum balances /= 2000 then
    error "Error: Checksum problem in account balances"
  else
    putStrLn "Did checksum total balances are OK!"
