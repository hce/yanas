module Command where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad

import Types

makeChanListener :: Chan a -> IO (ThreadId, TVar [a])
makeChanListener chan = do
  tvar <- atomically $ newTVar []
  threadId <- forkIO $ listen chan tvar
  return (threadId, tvar)
  where
    listen :: Chan a -> TVar [a] -> IO ()
    listen c v = forever $ do
      item <- (:[]) <$> readChan c
      atomically $ do
        items <- readTVar v
        writeTVar v $ items ++ item
    
getItem :: TVar [a] -> IO (Maybe a)
getItem a = atomically $ do
  items <- readTVar a
  case items of
    []     -> return Nothing
    (x:xs) -> writeTVar a xs >> return (Just x)
      
getItems :: TVar [a] -> IO [a]
getItems a = atomically $ do
  items <- readTVar a
  writeTVar a []
  return items
