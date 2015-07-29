object NumberConverter {
  val table = Map(
    1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten",
    11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 15 -> "fifteen",
    20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty", 60 -> "sixty", 70 -> "seventy", 80 -> "eighty", 90 -> "ninety"
  )

  def toWords(n: Int): String = {
    val ns = n.toString.map(_.asDigit).toList
    ns.indices.reverse.map(i => {
      val num = ns((ns.length - 1) - i)
      convert(num, i, ns.indices.length - 1)
    }).mkString(" ")
  }

  def convert(n: Int, i: Int, length: Int): String = {
    if (i == 0 && n != 0) table.get(n).get
    else if (i == 1) {
      if (length > 2) "and " + table.get(n * 10).get + "-"
      else table.get(n * 10).get
    }
    else if (i == 2) table.get(n).get + " hundred"
    else if (i == 3) table.get(n).get + " thousand"
    else ""
  }
}

NumberConverter.toWords(12)
