object NumberConverter {
  private val table = Map(
    1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine",
    10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen",
    17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen", 20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty",
    60 -> "sixty", 70 -> "seventy", 80 -> "eighty", 90 -> "ninety"
  )

  def toWords(n: Int): String = {
    val ns = n.toString.map(_.asDigit).toList
    ns.indices.reverse.map(i => {
      val num = ns((ns.length - 1) - i)
      convert(num, i, ns.indices.length, ns)
    }).mkString(" ").replaceAll("[ -]", "")
  }

  private def convert(n: Int, i: Int, length: Int, nums: List[Int]): String = {
    if (i == 1 && n == 1) {
      if (length > 2) "and " + table.get(n * 10 + nums(i + 1)).get
      else table.get(n * 10 + nums(1)).get
    }
    else if (i == 0 && n != 0) {
      if (length > 2 && nums(i + 1) == 1) ""
      else if (length == 2 && nums.head == 1) ""
      else table.get(n).get
    }
    else if (i == 1) {
      if (length > 2) {
        if (n == 0) {
          if (nums(i + 1) == 0) ""
          else "and "
        }
        else "and " + table.get(n * 10).get + "-"
      }
      else table.get(n * 10).get
    }
    else if (i == 2) {
      if (n == 0) ""
      else table.get(n).get + " hundred"
    }
    else if (i == 3) {
      if (n == 0) ""
      else table.get(n).get + " thousand"
    }
    else ""
  }
}

assert(NumberConverter.toWords(1) == "one")
assert(NumberConverter.toWords(10) == "ten")
assert(NumberConverter.toWords(12) == "twelve")
assert(NumberConverter.toWords(20) == "twenty")
assert(NumberConverter.toWords(21) == "twentyone")
assert(NumberConverter.toWords(34) == "thirtyfour")
assert(NumberConverter.toWords(100) == "onehundred")
assert(NumberConverter.toWords(110) == "onehundredandten")
assert(NumberConverter.toWords(111) == "onehundredandeleven")
assert(NumberConverter.toWords(115) == "onehundredandfifteen")
assert(NumberConverter.toWords(121) == "onehundredandtwentyone")
assert(NumberConverter.toWords(104) == "onehundredandfour")
assert(NumberConverter.toWords(1000) == "onethousand")
assert(NumberConverter.toWords(1342) == "onethousandthreehundredandfortytwo")


1.to(1000).map(NumberConverter.toWords).map(_.length).sum
