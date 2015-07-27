def largestPrimeFactor(n: BigInt): BigInt = {
  def loop(n: BigInt, lpf: BigInt): BigInt = {
    if (n <= lpf) lpf
    else {
      if (n % lpf == BigInt(0)) loop(n / lpf, 2)
      else loop(n, lpf + 1)
    }
  }
  loop(n, 2)
}

largestPrimeFactor(BigInt("600851475143"))
