package snippets.gsoc2024



/*
def myFun: IO[HttpReply] = {
  for {result1 <- talkToServer("request1", None)
       _ <- IO.sleep(100.millis)
       results2 <- talkToServer("request2", Some(results1.data))
       _ <- if results2.isOk then
              for { _ <- writeToFile(results2.data)
                    _ <- IO.println("done")
              } yield ()
            else
              IO.println("abort")
       } yield results2
}


def myFun1(using RunContext): IO[HttpReply] = async[IO] {
  val results1 = await(talkToServer("request1", None))
  await(IO.sleep(100.millis))
  val results2 = await(talkToServer("request2", Some(results1.data)))
  if results2.isOk then
    await(writeToFile(results2.data))
    await(IO.println("done"))
  else
    await(IO.println("abort"))
  results2
}


def myFun2(using RunContext): IO[HttpReply] = async[IO] {
  val results1 = talkToServer("request1", None)
  IO.sleep(100.millis)
  val results2 = await(talkToServer("request2", Some(results1.data)))
  if results2.isOk then
    writeToFile(results2.data)
    IO.println("done")
  else
    IO.println("abort")
  results2
}


def write[A](a: A)(using Writer[A]): Unit

trait Writers {
  given Writer[Int] ...
  given Writer[String] ...
}

def withJsonWriters[A](f: Writes ?=> A): A

withJsonWriters {
  write(1)
  write("abc")
}

def readFirstN1(urls: Seq[String])(using CpsDirect[Future]): Seq[String =
  urls.map(url => fetch(url))


def readFirstN(urls: Seq[String])(using CpsDirect[Future]): Seq[String] =
  urls.map(url =>
    asynchronized(fetch(url))
  ).map{
    future => await(future)
  }


def whileHelper[F:CpsMonad](c:F[Boolean])(body: F[Unit]): F[Unit] =
  F.flatMap(c) { b =>
    if b then
      F.flatMap(body) { _ => whileHelper(c)(body) }
    else
      F.pure(())
  }

*/
