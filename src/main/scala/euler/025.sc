def fibonacci: Stream[BigInt] = {
  def loop(a: BigInt, b: BigInt): Stream[BigInt] =
    (a + b) #:: loop(b, a + b)
  1 #:: loop(0, 1)
}

var index = 0
fibonacci.takeWhile(p => {
  index = index + 1
  p.toString().length < 1000
}).toList

index
