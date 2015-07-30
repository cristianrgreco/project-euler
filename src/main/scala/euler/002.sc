def fibonacci: Stream[Int] = {
  def loop(a: Int, b: Int): Stream[Int] = (a + b) #:: loop(b, b + a)
  loop(0, 1)
}

fibonacci.takeWhile(x => x < 4000000).filter(x => x % 2 == 0).sum
