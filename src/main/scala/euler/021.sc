def factors(n: Int): List[Int] = {
  def loop(i: Int, acc: List[Int]): List[Int] = {
    if (i == n) acc
    else {
      if (n % i == 0) loop(i + 1, acc :+ i)
      else loop(i + 1, acc)
    }
  }
  loop(1, List())
}


val limit = 10000

1.until(limit).map(i => (i, factors(i).sum)).toMap
  .filter(_._2 != 0)
  .filter(p => factors(p._2).sum == p._1)
  .filter(p => p._1 != p._2)
  .map(p => p._1 + p._2)
  .toList
  .distinct
  .sum
