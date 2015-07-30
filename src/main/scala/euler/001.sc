def sumOfMultiples(n: Int, multiples: List[Int]): Int = {
  (for (multiple <- multiples; i <- 1 until n if i % multiple == 0) yield i).distinct.sum
}

sumOfMultiples(1000, List(3, 5))
