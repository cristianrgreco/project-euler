def latticePathsForGrid(n: BigInt, k: BigInt): BigInt = {
  def binomialCoefficient(n: BigInt, k: BigInt): BigInt =
    factorial(n) / (factorial(k) * factorial(n - k))

  def factorial(n: BigInt): BigInt = {
    def loop(n: BigInt, acc: BigInt): BigInt =
      if (n <= 1) acc
      else loop(n - 1, acc * n)
    loop(n, 1)
  }

  binomialCoefficient(n + k, k)
}

latticePathsForGrid(20, 20)
