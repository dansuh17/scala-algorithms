package my.algorithm

object StrangePrinter extends App {
  println(StrangePrinterSolution.strangePrinter("aaaaaaaaaaaaaaaaaaaaa"))
  println(StrangePrinterSolution.strangePrinter("aba"))
}

object StrangePrinterSolution {
  def strangePrinter(s: String): Int = {
    // compress the string by removing consecutive duplicate characters
    val newString: String = s.head + ((s zip s.tail) collect {
      case (a, b) if a != b => b
    }).foldLeft("")((acc, next) => acc + next)

    val n = newString.length
    val arr = Array.tabulate(n, n)((_, _) => -1)

    def fill(arr: Array[Array[Int]], start: Int, end: Int): Int = {
      if (start == end) {
        1
      } else if (start > end) {
        0
      } else if (arr(start)(end) != -1) {
        arr(start)(end)
      } else {
        val naiveCost = fill(arr, start, end - 1) + 1
        val endChar = newString(end)
        val subCosts =
          (start until end)
            .filter(i => endChar == newString(i))
            .foldRight(List.empty[Int])((i, acc) => {
              val subCost = fill(arr, start, i - 1) + fill(arr, i + 1, end)
              subCost :: acc
            })

        val allCosts = naiveCost :: subCosts
        val minCost = allCosts.min

        arr(start)(end) = minCost // memoize
        minCost
      }
    }

    fill(arr, 0, n - 1)
  }
}
