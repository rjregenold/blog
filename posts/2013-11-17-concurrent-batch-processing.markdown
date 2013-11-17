---
title: Concurrent Batch Processing
---

At my day job I often find myself needing to pull a bunch of data from the
database and process each row. The consumer is generally really slow, executing
either a bunch of IO bound or CPU bound tasks on each row (downloading files, 
resizing images, processing epubs, uploading files, etc). 

<!--more-->

This is very easy to do in serial. Many of these processes started off in Ruby:

```ruby
Book.find_each do |book|
  do_something_slow(book)
end
```

Of course, as the data grows this becomes unrealistic. I then went down the
path of really over-engineering this thing, using [Akka](http://akka.io/) and 
the actor-model to pull data and distribute it to workers. My thinking was that 
as things grew, we could spin-up extra boxes, run workers on those boxes and 
distribute the work to all those new workers. This worked pretty well, but I 
didn't really like how all the messages passed around in Akka are untyped. Also, 
I ended up with a large, complex actor based system that is now very difficult 
to reason about and make changes to.

I\'ve been learning/using Haskell for personal projects for the past 2 years.
I spent a little time one evening trying to figure out how I would solve this
in Haskell. I came across [this paste](http://lpaste.net/79863) which presents
a really clean way to concurrently process batches of data. I've basically
plagiarized it for my own purposes.

Below is a full skeleton example that uses the
[optparse-applicative](http://hackage.haskell.org/package/optparse-applicative)
package to handle command line args and 
[mysql-simple](http://hackage.haskell.org/package/mysql-simple) to query
a MySQL database.

``` {.haskell .numberLines}
{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad
import Data.List (intercalate)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Options.Applicative
import System.Random (randomRIO)


main :: IO ()
main = execParser opts >>= run

--------------------------------------------------------------------------------
-- args parsing
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- logging service (from Parallel and Concurrent Programming in Haskell)
--------------------------------------------------------------------------------

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

--------------------------------------------------------------------------------
-- data
--------------------------------------------------------------------------------

dbConnInfo :: ConnectInfo
dbConnInfo = defaultConnectInfo
  { connectDatabase = "my_database"
  }

newtype BookId = BookId { unBookId :: Integer }

instance QueryResults BookId where
  convertResults [fa] [va] = BookId a
    where
      !a = convert fa va
  convertResults fs vs = convertError fs vs 1

--------------------------------------------------------------------------------
-- workers
--------------------------------------------------------------------------------

seconds :: Int -> Int
seconds = (*(10^6))

workerThread :: Logger -> BookId -> IO ()
workerThread log bookId = do
  x <- randomRIO (5, 10)
  -- simulate a long running process
  threadDelay $ seconds x
  logMessage log $ "finished book id: " ++ show (unBookId bookId)

spawnWorker :: Logger -> TVar Int -> TMVar () -> BookId -> IO ()
spawnWorker log taskstv donemv bookId = 
  void $ forkIO ((incTasks >> workerThread log bookId) `finally` decTasks)
  where
    incTasks = atomically $ modifyTVar' taskstv (+1)
    decTasks = atomically $ do
      tasks <- readTVar taskstv
      let !newTasks = tasks - 1
      writeTVar taskstv newTasks
      when (newTasks < 1) . void $ tryPutTMVar donemv ()

runPreviews :: Integer -> IO ()
runPreviews pId = do
  taskstv <- newTVarIO (0 :: Int)
  donemv <- newEmptyTMVarIO :: IO (TMVar ())
  log <- initLogger
  conn <- connect dbConnInfo
  let worker = spawnWorker log taskstv donemv
  forEach conn "select id from books where publisher_id = ?" (Only pId) worker
  close conn
  loop log taskstv donemv
  logStop log
  where
    yepNope = maybe "Nope" (const "Yep")
    loop log taskstv donemv = do
      done <- atomically $ tryTakeTMVar donemv
      tasks <- atomically $ readTVar taskstv
      logMessage log $ intercalate "\t"
        [ "done = " ++ yepNope done
        , "tasks = " ++ show tasks
        ]
      if (isJust done)
        then return ()
        else do
          threadDelay $ seconds 1
          loop log taskstv donemv
```

Once this is compiled, it can be run like:

```bash
$ myapp previews 6
```

The real concurrent processing action happens on Lines 98+. Line 125 creates
a new `TVar Int` to keep track of the number of workers currently working.
Line 126 creates a `TMVar ()` that will contain a result once all the
concurrent tasks have completed. These are passed to the `spawnWorker`
function. This function is responsible for spawning new worker threads, handling 
the bookkeeping for the number of workers still working and populating `donemv`
when all the workers are finished.

Once all the workers are spawned, the `runPreviews` function enters a loop that
checks the status every 1 second. It prints some stats to stdout and continues
to loop. Once all the workers are complete it terminates the loop.

Lines 45-79 define a simple logging service that is just responsible for
keeping stdout sane.

This still loads all the book ids to be processed into memory at once. This
does not seem to be too big of a problem yet. I'm doing this because the
documentation for `forEach` in mysql-simple says the consumer must return
quickly to prevent tying up server resources and preventing other clients from
updated the tables that are streaming results. If I find a better way I will
blog about it.
