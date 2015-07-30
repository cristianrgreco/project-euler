def sumOfDivisors(n: BigInt, proper: Boolean): BigInt = {
  def calculate(coefficient: Int, exponent: Int): BigInt = {
    val x = BigInt(coefficient).pow(exponent + 1) - 1
    if (coefficient > 2) x / (coefficient - 1)
    else x
  }

  val primes = primeFactors(n).groupBy(identity).values.map(l => (l.length, l))

  val result = primes.map(f => {
    val coefficient = f._2.head
    val exponent = f._1
    calculate(coefficient, exponent)
  }).product

  if (proper) result - n
  else result
}

def primeFactors(n: BigInt): List[Int] = {
  def loop(n: BigInt, lpf: Int, acc: List[Int]): List[Int] = {
    if (lpf > n) acc
    else {
      if (n % lpf == BigInt(0)) loop(n / lpf, 2, acc :+ lpf)
      else loop(n, lpf + 1, acc)
    }
  }
  loop(n, 2, List())
}

def isAbundant(n: BigInt, sumOfDivisors: BigInt): Boolean = sumOfDivisors > n


val limit = 28123

val abundantNumbers = 1.to(limit).filter(i => isAbundant(i, sumOfDivisors(i, proper = true))).toVector
val sumsOfAbundantNumbers = abundantNumbers.flatMap(i => {
  abundantNumbers.indices.map(j => {
    i + abundantNumbers(j)
  })
}).toSet

1.to(limit).filterNot(sumsOfAbundantNumbers.contains).sum
