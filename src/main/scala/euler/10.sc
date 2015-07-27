object Exercise {
  val primes: Stream[Int] =
    2 #:: Stream.from(3, 2)
      .filter(i => primes.takeWhile(j => j * j <= i)
      .forall(k => i % k > 0))

  def sumPrimesUpTo(n: Int): BigInt = {
    primes.takeWhile(p => p < n).map(num => BigInt(num)).sum
  }
}

Exercise.sumPrimesUpTo(2000000)
