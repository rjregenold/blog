---
title: Concurrent Batch Processing, Part 2
---

In my [previous post](/posts/2013-11-17-concurrent-batch-processing.html)
I showed a way to process a batch of data concurrently using Haskell. One
drawback to that approach was that all the data to be processed was loaded into
memory. This evening I found a better way that only loads a small subset of
data into memory at a time.

<!--more-->

Here is what the function looks like:

``` {.haskell .numberLines}
processConcurrently_ :: (QueryParams q, QueryResults r, HasNextOffset r)
                     => (Integer -> (Query, q))
                     -> (r -> IO ())
                     -> Connection
                     -> IO ()
processConcurrently_ f action conn = loop 0
  where
    loop offset = do
      let (q, qs) = f offset
      xs <- query conn q qs
      unless (null xs) (process xs)
    process xs = do
      taskstv <- newTVarIO 0
      bar <- newBarrier
      let worker = spawnWorker taskstv bar
      mapM_ (worker . action) xs
      watchStatus taskstv bar
      loop (fromMaybe 0 (nextOffset xs))
    watchStatus taskstv bar = do
      done <- atomically $ isBarrierOpen bar
      tasks <- atomically $ readTVar taskstv
      infoM logName $ intercalate "\t"
        [ "done=" ++ yepNope done
        , "tasks=" ++ show tasks
        ]
      unless done $ do
        threadDelay (seconds 1)
        watchStatus taskstv bar
```

The first argument (line 2) is a function that takes an offset and returns a tuple
containing a query and parameters. This gives the caller full control over the
construction of the query. An implementation for this function might look like:

```haskell
mkQuery offset = (q, qs)
  where
    q = "select id from users where id > ? order by id limit ?"
    qs = (offset, 1000 :: Int)
```

This generates a query that will select the next 1000 users above the given
offset. It is important to ensure the results are ordered by `id` so that
increasing the offset returns the next logical set of users.

The second argument (line 3) is a function that accepts a query result and
processes it. This action will be run in separate thread so that several of
them will be running concurrently. An implementation for this functiono might
look like:

```haskell
userAction :: UserId -> IO ()
userAction userId =
  debugM logName $ "processing user id: " ++ show (unUserId userId)
```

This function is passed into ```processConcurrently_```, so it can close over any
other environment parameters it might need (database connections, etc). I will
show an example of that at the end of this post.

The body of ```processConcurrently_``` starts off by calling `loop` with an
initial offset of 0 (line 6). The first thing `loop` does is call the given
query builder function (line 2) with the given offset to generate a query and
appropriate params. That query is then executed (line 10) and then hits a guard
at line 11. If the result set is empty, it is done. Otherwise, it processes the
given batch of results.

The `process` function works more or less the same as it did in my previous
post. The `newBarrier` call on line 14 deserves a little explanation. The idea
is largely based on [this post by Neil
Mitchell](http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html).
A barrier is just a TMVar that is read several times and written to once
(signaling completion). My implementation looks like:

```haskell
newtype Barrier a = Barrier { unBarrier :: TMVar a }

newBarrier :: IO (Barrier a)
newBarrier = Barrier <$> newEmptyTMVarIO

signalBarrier :: Barrier a -> a -> STM ()
signalBarrier = fmap void . tryPutTMVar . unBarrier

checkBarrier :: Barrier a -> STM (Maybe a)
checkBarrier (Barrier bar) =
  traverse (\x -> tryPutTMVar bar x >> return x) =<< tryTakeTMVar bar

-- | The barrier will be open when it has been signalled as complete.
isBarrierOpen :: Barrier a -> STM Bool
isBarrierOpen = fmap (maybe False (const True)) . checkBarrier
```

Nothing fancy here. Just a `newtype` wrapper for a `TMVar` to help clarify the
purpose of said `TMVar`.

Back to the original function: the `spawnWorker` function (line 15) remained
very much the same. It looks like:

```haskell
spawnWorker :: TVar Int   -- ^ count of tasks currently running
            -> Barrier () -- ^ have all the workers finished
            -> IO ()      -- ^ an action to run in a separate thread
            -> IO ()
spawnWorker taskstv bar action =
  void $ forkIO ((incTasks >> action) `finally` decTasks)
  where
    incTasks = atomically $ modifyTVar' taskstv (+1)
    decTasks = atomically $ do
      tasks <- readTVar taskstv
      let !newTasks = tasks - 1
      writeTVar taskstv newTasks
      when (newTasks < 1) (signalBarrier bar ())
```

The only difference is that the action is now passed in as an IO action to be
executed on a separate thread.

The `watchStatus` function (line 19) stayed the same (outside of swapping the
logging service for [hslogger](http://hackage.haskell.org/package/hslogger)).

Once the `watchStatus` function completes, `loop` is called again with the next
offset. To allow the next offset to be determined, I created a `HasNextOffset`
typeclass.

```haskell
class HasNextOffset x where
  nextOffset :: [x] -> Maybe Integer
```

It takes a batch of results and possibly returns the next offset. An
implementation might look like:

```haskell
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

newtype UserId = UserId { unUserId :: Integer }

instance HasNextOffset UserId where
  nextOffset = fmap unUserId . maybeLast
```

#### Complete working example

Here is a full example of it all in action:

```haskell
{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Traversable
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Data.Pool
import Options.Applicative
import System.Log.Logger


logName :: String
logName = "concurrent-sample"

data Command
  = Previews Integer

previewOpts :: Parser Command
previewOpts = Previews 
  <$> argument auto 
      (  metavar "PUBLISHER_ID"
      <> help "Process books for PUBLISHER_ID"
      )

commands :: Parser Command
commands = subparser 
  ( command "previews"
    (info previewOpts
          (progDesc "Create previews"))
  )

run :: Command -> IO ()
run (Previews pId) = runPreviews pId

opts :: ParserInfo Command
opts = info (commands <**> helper) idm

main :: IO ()
main = do
  setupLogging
  execParser opts >>= run

setupLogging :: IO ()
setupLogging =
  updateGlobalLogger logName (setLevel DEBUG)

dbConnInfo :: ConnectInfo
dbConnInfo = defaultConnectInfo
  { connectDatabase = "my_database"
  }

newtype BookId = BookId { unBookId :: Integer }

data NavPoint = NavPoint
  { navPointId :: Integer
  }

instance QueryResults BookId where
  convertResults [fa] [va] = BookId a
    where
      !a = convert fa va
  convertResults fs vs = convertError fs vs 1

instance QueryResults NavPoint where
  convertResults [fa] [va] = NavPoint a
    where
      !a = convert fa va
  convertResults fs vs = convertError fs vs 1

instance HasNextOffset BookId where
  nextOffset = fmap unBookId . maybeLast

mkPool :: IO (Pool Connection)
mkPool = createPool (connect dbConnInfo) close 1 300 10

runPreviews :: Integer -> IO ()
runPreviews pId = do
  pool <- mkPool
  withResource pool $ processConcurrently_ mkQuery (previewsAction pool)
  where
    query' = "select b.id \
      \ from books as b \
      \ left outer join book_previews as bp on bp.book_id = b.id \
      \ left outer join nav_points as np on np.book_id = b.id \
      \ where \
      \   b.import_publisher_id = ? and \
      \   bp.id is null and \
      \   np.id is not null and \
      \   b.id > ? \
      \ group by b.id \
      \ order by b.id \
      \ limit ?"
    mkQuery offset = (query', (pId, offset, 1000 :: Int))

previewsAction :: (Pool Connection) -> BookId -> IO ()
previewsAction pool bookId = withResource pool $ \conn -> do
  ss <- query conn "select id from nav_points where book_id = ?" (Only . unBookId $ bookId)
  debugM logName $ "book id: " ++ show (unBookId bookId)
  debugM logName $ "nav points: " ++ show (length (ss :: [NavPoint]))

class HasNextOffset x where
  nextOffset :: [x] -> Maybe Integer

newtype Barrier a = Barrier { unBarrier :: TMVar a }

newBarrier :: IO (Barrier a)
newBarrier = Barrier <$> newEmptyTMVarIO

signalBarrier :: Barrier a -> a -> STM ()
signalBarrier = fmap void . tryPutTMVar . unBarrier

checkBarrier :: Barrier a -> STM (Maybe a)
checkBarrier (Barrier bar) =
  traverse (\x -> tryPutTMVar bar x >> return x) =<< tryTakeTMVar bar

isBarrierOpen :: Barrier a -> STM Bool
isBarrierOpen = fmap (maybe False (const True)) . checkBarrier

spawnWorker :: TVar Int
            -> Barrier ()
            -> IO ()
            -> IO ()
spawnWorker taskstv bar action =
  void $ forkIO ((incTasks >> action) `finally` decTasks)
  where
    incTasks = atomically $ modifyTVar' taskstv (+1)
    decTasks = atomically $ do
      tasks <- readTVar taskstv
      let !newTasks = tasks - 1
      writeTVar taskstv newTasks
      when (newTasks < 1) (signalBarrier bar ())

processConcurrently_ :: (QueryParams q, QueryResults r, HasNextOffset r)
                     => (Integer -> (Query, q))
                     -> (r -> IO ())
                     -> Connection
                     -> IO ()
processConcurrently_ f action conn = loop 0
  where
    loop offset = do
      let (qry, qs) = f offset
      xs <- query conn qry qs
      unless (null xs) (process xs)
    process xs = do
      taskstv <- newTVarIO 0
      bar <- newBarrier
      let worker = spawnWorker taskstv bar
      mapM_ (worker . action) xs
      watchStatus taskstv bar
      loop (fromMaybe 0 (nextOffset xs))
    watchStatus taskstv bar = do
      done <- atomically $ isBarrierOpen bar
      tasks <- atomically $ readTVar taskstv
      infoM logName $ intercalate "\t"
        [ "done=" ++ yepNope done
        , "tasks=" ++ show tasks
        ]
      unless done $ do
        threadDelay (seconds 1)
        watchStatus taskstv bar

--------------------------------------------------------------------------------
-- utils
--------------------------------------------------------------------------------

seconds :: Int -> Int
seconds = (*(10^6))

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

yepNope :: Bool -> String
yepNope True  = "Yep"
yepNope False = "Nope"
```

In the `previewsAction` function you can see how I curried the function so that
the pool is closed over and I can access the database from my worker thread.
This same pattern could be used to pass around an API connection, file handle,
etc.

#### Conclusion

The only thing I don\'t like about this approach is the lack of
type safety around building the queries and query parameters, but that is more
of an issue with low(ish)-level database access in Haskell. Overall, this
should work pretty well.
