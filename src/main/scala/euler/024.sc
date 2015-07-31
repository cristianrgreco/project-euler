def getPermutation[T](input: List[T], n: Int): List[T] = {
  val permutations = input.permutations
  def loop(i: Int, result: List[T]): List[T] = {
    if (i == n) result
    else loop(i + 1, permutations.next())
  }

  loop(0, List())
}


val digits = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
getPermutation(digits, 1000000).mkString
