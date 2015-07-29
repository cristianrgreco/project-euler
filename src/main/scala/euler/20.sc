def factorial(n: BigInt): BigInt = {
  def loop(n: BigInt, acc: BigInt): BigInt =
    if (n <= 1) acc
    else loop(n - 1, acc * n)
  loop(n, 1)
}

def sumOfDigits(n: BigInt) = n.toString().map(_.asDigit).sum

sumOfDigits(factorial(100))
