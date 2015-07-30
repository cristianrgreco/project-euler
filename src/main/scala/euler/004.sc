def palindromeProduct = {
  val products = (for {
    i <- 100 to 999
    j <- 100 to 999
    x = i * j
  } yield x).toList

  def isPalindrome(n: Int): Boolean = {
    val ns = n.toString
    val nsLength = ns.length
    val nsLengthHalf = ns.length / 2
    val isOdd = nsLength % 2 != 0

    val leftHalf = ns.substring(0, nsLengthHalf)
    val rightHalf =
      if (isOdd) ns.substring(nsLengthHalf + 1, nsLength)
      else ns.substring(nsLengthHalf, nsLength)

    leftHalf == rightHalf.reverse
  }

  products.filter(isPalindrome).max
}

palindromeProduct
