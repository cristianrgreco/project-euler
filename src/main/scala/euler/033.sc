def factorial(n: Int): BigInt = {
  def loop(n: BigInt, acc: BigInt): BigInt =
    if (n == BigInt(1)) acc
    else loop(n - 1, acc * n)
  loop(n, 1)
}

def factorialForDigits(n: BigInt): Seq[BigInt] = {
  n.toString().map(_.asDigit).map(factorial)
}

val upperBound = factorial(9) * 7
BigInt(3).to(200).map(factorialForDigits)
