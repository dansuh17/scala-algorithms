package my.algorithm

object MaxChunksToMakeSorted extends App {
  println(MaxChunksToMakeSortedSolution.maxChunksToSorted(Array(4, 3, 2, 1, 0)))
  println(MaxChunksToMakeSortedSolution.maxChunksToSorted(Array(1, 0, 2, 3, 4)))
}

object MaxChunksToMakeSortedSolution {
  def maxChunksToSorted(arr: Array[Int]): Int = {
    arr
      .foldLeft((-1, 0, 0))((acc, next) => {
        val max = if (next > acc._1) next else acc._1
        val count = acc._2

        val partition = if (max == count) {
          acc._3 + 1
        } else {
          acc._3
        }

        (max, count + 1, partition)
      })
      ._3
  }
}
