package my.algorithm

object MaxChunksToMakeSorted2 extends App {}

object MaxChunkToMakeSortedSolution {
  def maxChunksToSorted(arr: Array[Int]): Int = {
    val sorted = arr.sorted
    val posMap: Map[Int, Int] =
      sorted
        .foldLeft((Map[Int, Int](), 0))((acc, next) => {
          val map = acc._1
          val accCnt = acc._2

          map.get(next) match {
            case Some(_) => (map + (next -> (accCnt + 1)), accCnt)
            case None    => (map + (next -> (accCnt + 1)), accCnt)
          }
        })
        ._1

  }
}
