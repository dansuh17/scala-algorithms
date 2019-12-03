package my.algorithm

/**
  * 72. Edit Distance
  * https://leetcode.com/problems/edit-distance/
  */
object EditDistance extends App {
  print(EditDistanceSolution.minDistance("abc", "aaa"))
}

object EditDistanceSolution {
  def minDistance(word1: String, word2: String): Int = {
    val N = word1.length + 1 // empty string
    val M = word2.length + 1
    val costs: Array[Array[Int]] = Array.ofDim[Int](N, M)
    costs(0)(0) = 0

    // select indices to loop over
    val indices = for {
      n <- 0 until N
      m <- 0 until M
      if n != 0 || m != 0 // exclude starting position
    } yield (n, m)

    indices.foreach(s => {
      val n = s._1
      val m = s._2
      var local_costs: List[Int] = List()
      if (n > 0) {
        val cost_add = costs(n - 1)(m) + 1
        local_costs = cost_add :: local_costs
      }

      if (m > 0) {
        val cost_del = costs(n)(m - 1) + 1
        local_costs = cost_del :: local_costs
      }

      if (n > 0 && m > 0) {
        val add_cost = if (word1(n - 1) == word2(m - 1)) 0 else 1
        val cost_replace = costs(n - 1)(m - 1) + add_cost
        local_costs = cost_replace :: local_costs
      }

      // min value among all cases
      costs(n)(m) = local_costs.min
    })

    costs(N - 1)(M - 1)
  }
}
