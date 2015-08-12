def isNarcissisticNumber(n: Int, pow: Int): Boolean = {

  def powerOfDigits: Int =
    n.toString.map(_.asDigit).map(i => math.pow(i, pow)).sum.toInt

  n == powerOfDigits

}


2.to(355500).filter(i => isNarcissisticNumber(i, 5)).sum
