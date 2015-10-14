object Strings {
  def permutations(s: String): Seq[String] = {
    def loop(curr: Char, rem: String, res: Seq[String]): Seq[String] = {
      if (rem.isEmpty) res.flatMap(s => permute(s, curr)).sorted
      else loop(rem.head, rem.tail, res.flatMap(s => permute(s, curr)))
    }
    def permute(s: String, c: Char): Seq[String] = {
      0.to(s.length).map(i => s.substring(0, i) + c + s.substring(i))
    }
    loop(s(1), s.substring(2), Seq(s.head.toString))
  }
}
Strings.permutations("ABC")
object Palindromes {
  def isPalindrome(s: String): Boolean = {
    if (s.length == 0) false
    else if (s.length == 1) true
    else if (s.length == 2) s(0) == s(1)
    else s(0) == s(s.length - 1) && isPalindrome(s.substring(1, s.length - 1))
  }
  def isPalindrome[T <: AnyVal](n: T): Boolean = isPalindrome(n.toString)
}
Palindromes.isPalindrome("ABABA")
Palindromes.isPalindrome(12321)
object Factors {
  def all(n: BigInt): Seq[BigInt] = {
    def loop(i: BigInt, factors: Seq[BigInt]): Seq[BigInt] = {
      if (i > math.sqrt(n.toDouble).toInt) factors
      else if (n % i == BigInt(0)) {
        val isPerfectSquare = n / i == i
        if (isPerfectSquare) loop(i + 1, factors :+ i)
        else loop(i + 1, factors :+ i :+ n / i)
      }
      else loop(i + 1, factors)
    }
    loop(BigInt(1), Seq.empty).sorted
  }
  def primes(n: BigInt): Seq[BigInt] = {
    def loop(i: BigInt, rem: BigInt, factors: Seq[BigInt]): Seq[BigInt] = {
      if (rem == BigInt(1)) factors
      else if (i > math.sqrt(n.toDouble).toInt) factors :+ rem
      else if (rem % i == BigInt(0)) loop(2, rem / i, factors :+ i)
      else loop(i + 1, rem, factors)
    }
    loop(BigInt(2), n, Seq.empty)
  }
}
Factors.all(BigInt("600851475143"))
Factors.primes(BigInt("600851475143"))
object Collections {
  def partition(s: Seq[Any], size: Int): Seq[Seq[Any]] = {
    def loop(rem: Seq[Any], res: Seq[Seq[Any]]): Seq[Seq[Any]] = {
      if (rem.isEmpty) res
      else loop(rem.drop(size), res :+ rem.take(size))
    }
    if (size <= 0) Seq(s)
    else loop(s, Seq.empty)
  }
}
Collections.partition(Seq(1, 2, 3, 4), 2)
Collections.partition(Seq(1, 2, 3, 4), 4)
object Math {
  def factorial(n: BigInt): BigInt = {
    def loop(n: BigInt, acc: BigInt): BigInt = {
      if (n == BigInt(1)) acc
      else loop(n - 1, acc * n)
    }
    loop(n, BigInt(1))
  }
  def triangularNumber(n: BigInt): BigInt = {
    n * (n + 1) / 2
  }
}
Math.factorial(5)
Math.triangularNumber(5)