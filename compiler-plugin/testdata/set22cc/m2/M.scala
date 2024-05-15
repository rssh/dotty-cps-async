

trait Monad[F[_]]:
  def flatMap[T,S](ft:F[T])(f: T=>F[S]): F[S]
  def pure[T](t:T): F[T]

  def map(T,S)(ft:F[T])(f: T=>S): F[S] = flatMap(ft)(t => pure(f(t)))
  def flatten(T)(ft:F[F[T]]): F[T] = flatMap(ft)(identity)

  def flatMap1(T,S)(ft:F[T])(f: T=>F[S]): F[S] = {
    // define  via map and flatten
    map(f)
    flatten(map(ft)(f))
  }