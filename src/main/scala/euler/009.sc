def pythagoreanTripletFor(x: Int): (Int, Int, Int) = {
  val xsq = math.sqrt(x).toInt

  for (n <- 1 to xsq) {
    for (m <- (n + 1) until xsq) {
      val a = (m * m) - (n * n)
      val b = 2 * m * n
      val c = (m * m) + (n * n)
      if (a + b + c == x) return (a, b, c)
    }
  }

  throw new RuntimeException("No triplet found for " + x)
}

val (a, b, c) = pythagoreanTripletFor(1000)
val result = a * b * c
