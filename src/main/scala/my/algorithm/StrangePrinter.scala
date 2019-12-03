package my.algorithm

object StrangePrinter

object A extends App {
  println(StrangePrinterSolution.strangePrinter("aaaaaaaaaaaaaaaaaaaaa"))
  println(StrangePrinterSolution.strangePrinter("aba"))
}

object StrangePrinterSolution {
  def strangePrinter(s: String): Int = {
    val n = s.length
    val arr: Array[Array[Int]] = Array.tabulate(n, n)((_, _) => -1)

    def fill(arr: Array[Array[Int]], start: Int, end: Int): Int = {
      if (start == end) {
        // println(s"${arr(start)(end)}, ${start}, ${end}")
        1
      } else if (start > end) {
        0
      } else if (arr(start)(end) != -1) {
        arr(start)(end)
      } else {
        val naiveCost = fill(arr, start, end - 1) + 1
        val endChar = s(end)
        val subCosts =
          (start until end)
            .filter(i => endChar == s(i))
            .foldRight(List.empty[Int])((i, acc) => {
              val subCost = fill(arr, start, i - 1) + fill(arr, i + 1, end)
              subCost :: acc
            })

        val allCosts = naiveCost :: subCosts
        val minCost = allCosts.min

        arr(start)(end) = minCost // memoize
        // println(s"${minCost}, ${start}, ${end}")
        minCost
      }
    }

    fill(arr, 0, n - 1)
  }
}
