package my.algorithm

// Lazy Fibonacci Sequence Generator

class Lazy[T](expr: => T) {
  lazy val value: T = expr
  def apply(): T = value
  def flatMap[A](f: T => Lazy[A]): Lazy[A] = Lazy { f(value).value }
  def map[A](f: T => A): Lazy[A] = Lazy { f(value) }
}

object Lazy { def apply[T](expr: => T): Lazy[T] = new Lazy({ expr }) }

object LazyFib {
  def fib(n: Int): Int = {
    def doFib(i: Int): Lazy[Int] = {
      if (i <= 2) Lazy(1)
      else
        Lazy {
          (for {
            a <- fibs(i - 1)
            b <- fibs(i - 2)
          } yield a + b).value
        }
    }

    lazy val fibs = Array.tabulate[Lazy[Int]](n)(doFib)
    doFib(n).value
  }
}
