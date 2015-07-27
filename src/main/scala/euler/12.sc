def numberOfFactorsFor(n: BigInt): BigInt = {
  def primeFactorsFor(n: BigInt): List[BigInt] = {
    def loop(n: BigInt, lpf: BigInt, acc: List[BigInt]): List[BigInt] =
      if (lpf > n) acc
      else if (n % lpf == BigInt(0)) loop(n / lpf, 2, acc :+ lpf)
      else loop(n, lpf + 1, acc)

    loop(n, 2, List())
  }

  val primeFactors = primeFactorsFor(n)
  val exponents = primeFactors
    .groupBy(identity)
    .values
    .map(list => list.length + 1)

  exponents.product
}

def triangleNumbersFor(n: BigInt): BigInt = {
  def loop(i: BigInt, acc: BigInt): BigInt =
    if (i <= 0) acc
    else loop(i - 1, acc + i)

  loop(n, 0)
}

val index = Stream.from(1).takeWhile(i => numberOfFactorsFor(triangleNumbersFor(i)) <= 500).last + 1
triangleNumbersFor(index)
