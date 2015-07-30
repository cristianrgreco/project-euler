def lowestCommonMultiple(r: Range): Int = {
  def lcm(a: Int, b: Int): Int = {
    (a * b) / gcd(a, b)
  }

  def gcd(a: Int, b:Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  r.reduce(lcm)
}

lowestCommonMultiple(1 until 20)
