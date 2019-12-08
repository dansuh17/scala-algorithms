package my.algorithm.epi

/**
  * Chapter 4. Primitive Types
  */
object CountBitsApp extends App {
  println(CountBits.countBits(3))
  println(CountBits.countBits(15))
  println(CountBits.countBits(1024))
}

object CountBits {
  def countBits(x: Int): Short = {
    @scala.annotation.tailrec
    def xShifts(x: Int, xs: Vector[Int]): Vector[Int] = {
      if (x != 0) xShifts(x >> 1, x +: xs)
      else xs
    }

    val xs = xShifts(x, Vector[Int]())
    xs.foldLeft(0)((acc, n) => acc + (n & 1)).toShort
  }
}
