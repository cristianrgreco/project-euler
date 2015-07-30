val primes: Stream[Int] =
  2 #:: Stream.from(3, 2)
    .filter(i => primes.takeWhile(j => j * j <= i)
    .forall(k => i % k > 0))

primes.take(10001).last
