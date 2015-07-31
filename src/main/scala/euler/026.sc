def lengthOfRecurrence(n: Int): Int = {
  def loop(i: Int): Int = {
    if (BigInt(10).pow(i).mod(n) == BigInt(1)) i
    else loop(i + 1)
  }
  if (isRepeating(n)) loop(1) else 0
}

def isRepeating(n: BigInt): Boolean = {
  val pfactors = primeFactors(n).toSet
  if (pfactors.contains(2) || pfactors.contains(5)) false else true
}

def primeFactors(n: BigInt): List[Int] = {
  def loop(n: BigInt, lpf: Int, acc: List[Int]): List[Int] = {
    if (lpf > n) acc
    else if (n % lpf == BigInt(0)) loop(n / lpf, 2, acc :+ lpf)
    else loop(n, lpf + 1, acc)
  }
  loop(n, 2, List())
}


2.until(1000).map(i => (i, lengthOfRecurrence(i))).maxBy(_._2)._1
