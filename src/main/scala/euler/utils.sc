object Strings {
  def permutations(s: String): Seq[String] = {
    def loop(curr: Char, rem: String, res: Seq[String]): Seq[String] = rem.length match {
      case 0 => res.flatMap(s => permute(s, curr)).distinct.sorted
      case _ => loop(rem.head, rem.tail, res.flatMap(s => permute(s, curr)))
    }
    def permute(s: String, c: Char): Seq[String] = {
      0.to(s.length).map(i => s.substring(0, i) + c + s.substring(i))
    }
    loop(s(1), s.substring(2), Seq(s.head.toString))
  }
}
Strings.permutations("ABC")

object Palindromes {
  def isPalindrome(s: String): Boolean = s.length match {
    case 0 => false
    case 1 => true
    case 2 => s(0) == s(1)
    case _ => s(0) == s(s.length - 1) && isPalindrome(s.substring(1, s.length - 1))
  }
  def isPalindrome[T <: AnyVal](n: T): Boolean = isPalindrome(n.toString)
}
Palindromes.isPalindrome("ABABA")
Palindromes.isPalindrome(12321)

object Factors {
  def all(n: BigInt): Seq[BigInt] = {
    def loop(i: BigInt, factors: Seq[BigInt]): Seq[BigInt] = i match {
      case _ if i > math.sqrt(n.toDouble).toInt => factors
      case _ if n % i == BigInt(0) => i match {
        case _ if n / i == i => loop(i + 1, factors :+ i)
        case _ => loop(i + 1, factors :+ i :+ n / i)
      }
      case _ => loop(i + 1, factors)
    }
    loop(BigInt(1), Seq.empty).sorted
  }
  def primes(n: BigInt): Seq[BigInt] = {
    def loop(i: BigInt, rem: BigInt, factors: Seq[BigInt]): Seq[BigInt] = rem match {
      case _ if rem == BigInt(1) => factors
      case _ if i > math.sqrt(n.toDouble).toInt => factors :+ rem
      case _ if rem % i == BigInt(0) => loop(2, rem / i, factors :+ i)
      case _ => loop(i + 1, rem, factors)
    }
    loop(BigInt(2), n, Seq.empty)
  }
  def count(n: BigInt): BigInt = {
    primes(n).groupBy(identity).map(_._2.length + 1).product
  }
}
Factors.all(BigInt("600851475143"))
Factors.primes(BigInt("600851475143"))
Factors.count(BigInt("600851475143"))

object Collections {
  def partition(s: Seq[Any], size: Int): Seq[Seq[Any]] = {
    def loop(rem: Seq[Any], res: Seq[Seq[Any]]): Seq[Seq[Any]] = rem match {
      case Nil => res
      case x :: xs => loop(rem.drop(size), res :+ rem.take(size))
    }
    if (size <= 0) Seq(s)
    else loop(s, Seq.empty)
  }
}
Collections.partition(Seq(1, 2, 3, 4), 2)
Collections.partition(Seq(1, 2, 3, 4), 4)

object Math {
  def fibonacci(n: BigInt): Seq[BigInt] = {
    def loop(a: BigInt, b: BigInt, i: BigInt, acc: Seq[BigInt]): Seq[BigInt] = i match {
      case _ if i == BigInt(0) => acc
      case _ => loop(b, a + b, i - 1, acc :+ a + b)
    }
    loop(0, 1, n - 1, Seq(0))
  }
  def factorial(n: BigInt): BigInt = {
    def loop(n: BigInt, acc: BigInt): BigInt = n match {
      case _ if n == BigInt(1) => acc
      case _ => loop(n - 1, acc * n)
    }
    loop(n, BigInt(1))
  }
  def triangularNumber(n: BigInt): BigInt = {
    n * (n + 1) / 2
  }
  def gcd(a: BigInt, b: BigInt): BigInt = b match {
    case _ if b == BigInt(0) => a
    case _ => gcd(b, a % b)
  }
  def lcm(a: BigInt, b: BigInt): BigInt = {
    (a * b) / gcd(a, b)
  }
}
Math.fibonacci(10)
Math.factorial(5)
Math.triangularNumber(5)
Math.gcd(10, 5)
Math.lcm(10, 5)
