package my.algorithm

/**
 * LeetCode 920. Number of Music Playlists
 * https://leetcode.com/problems/number-of-music-playlists/
 */
object NumberPlaylistsApp extends App {
  println(NumberPlaylists.numMusicPlaylists(2, 3, 0)) // 6
  println(NumberPlaylists.numMusicPlaylists(3, 3, 2))
  println(NumberPlaylists.numMusicPlaylists(2, 2, 2))
  println(NumberPlaylists.numMusicPlaylists(16, 16, 4)) // 789741546
  println(NumberPlaylists.numMusicPlaylists(37, 50, 8))
}

object NumberPlaylists {
  def numMusicPlaylists(N: Int, L: Int, K: Int): Int = {
    val dp: Array[Array[Long]] =
      Array.tabulate(N + 1, L + 1)((n, l) => if (l == n) 1L else 0L)
    val div = 1_000_000_007L
    // pre-calculate the factorial value
    val mult = (1 to N).foldLeft(1L)((acc, elem) => (acc * elem) % div)

    for {
      n <- (K + 1) to N
      l <- (K + 1) to L
      if l > n
    } {
      val v = dp(n - 1)(l - 1) + dp(n)(l - 1) * (n - K)
      dp(n)(l) = v % div
    }

    val x = dp(N)(L)

    ((x.toLong * mult) % div).toInt
  }
}
