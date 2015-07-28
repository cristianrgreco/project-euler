def collatzSequence(n: BigInt): List[BigInt] = {
  def even(n: BigInt): BigInt = n / 2
  def odd(n: BigInt): BigInt = 3 * n + 1

  def loop(x: BigInt, acc: List[BigInt]): List[BigInt] = {
    if (x == BigInt(1)) acc :+ BigInt(1)
    else {
      if (x % 2 == BigInt(0)) loop(even(x), acc :+ x)
      else loop(odd(x), acc :+ x)
    }
  }

  loop(n, List())
}

1.until(1000000)
  .flatMap(i => Map(i -> collatzSequence(i).length))
  .maxBy(_._2)._1
