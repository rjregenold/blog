---
title: Scalaz EitherT, Akka and Future
subtitle: An alternative to Akka Dataflow
---

The `scalaz.EitherT` monad transformer provides a nice way to cleanly 
coordinate a sequence of asynchronous operations that may fail. One place where
this comes in handy is in Akka when coordinating work between multiple actors.

For this post, I\'ll use the contrived example of needing to download a file,
run some process on it that generates a result and persists that result. Here
is what the end result looks like:

~~~ {.scala .numberLines}
type Result[A] = Throwable \/ A

def receive = {
  case DoWork(path) => 
    doWork(path).run.map(_.fold(
      WorkFail(_),
      WorkDone(_)
    )) pipeTo sender
}

def catchTimeout[A](f:Future[Result[A]]) = f.recover {
  case e => e.left
}

def eT[A](f:Future[Result[A]]) =
  EitherT(catchTimeout(f))

def doWork(path:String) = for {
  file <- eT((downloader ? DownloadFile(path)).mapTo[Result[File]])
  res1 <- eT((processor1 ? DoSomeWork(file)).mapTo[Result[Double]])
  res2 <- eT((db ? SaveResult(res1)).mapTo[Result[Answer])
} yield res2
~~~

<!-- more -->

First, we need to make `scala.concurrent.Future` into a `scalaz.Monad`
instance.

~~~ {.scala .numberLines}
implicit val futureMonad = new Monad[Future] {
  def bind[A, B](fa:Future[A])(f: A => Future[B]):Future[B] =
    fa.flatMap(f)

  def point[A](a: => A):Future[A] =
    Future(a)
}
~~~
