def powerDigitSum(coefficient: Int, exponent: Int): BigInt = {
  val power = BigInt(coefficient).pow(exponent)
  power.toString().map(_.asDigit).sum
}

powerDigitSum(2, 1000)
