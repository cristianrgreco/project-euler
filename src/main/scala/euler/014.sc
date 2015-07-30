import scala.collection.mutable

object Collatz {
  val memo = new mutable.HashMap[BigInt, List[BigInt]]

  def sequence(original: BigInt): List[BigInt] = {
    def even(n: BigInt): BigInt = n / 2
    def odd(n: BigInt): BigInt = 3 * n + 1

    def loop(n: BigInt, acc: List[BigInt]): List[BigInt] = {
      if (memo.contains(n)) {
        val completeList = acc ++ memo.get(n).get
        if (!memo.contains(original)) memo.put(original, completeList)
        completeList
      }
      else if (n == BigInt(1)) {
        val completeList = acc :+ BigInt(1)
        memo.put(original, completeList)
        completeList
      }
      else {
        if (n % 2 == BigInt(0)) loop(even(n), acc :+ n)
        else loop(odd(n), acc :+ n)
      }
    }

    loop(original, List())
  }
}

1.until(1000000)
  .flatMap(i => Map(i -> Collatz.sequence(i).length))
  .maxBy(_._2)._1
