def sumOfPowersUpTo(n: Int): BigInt =
  1.to(n).map(i => BigInt(i).pow(i)).sum

def getLastDigitsFromNumber(n: BigInt, d: Int): String = {
  val ns = n.toString()
  ns.substring(ns.length - d, ns.length)
}


getLastDigitsFromNumber(sumOfPowersUpTo(1000), 10)
