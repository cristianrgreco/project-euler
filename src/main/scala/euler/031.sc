import scala.collection.mutable

def changeFor(money: Int, coins: Seq[Int]): Int = {
  val partitions = mutable.HashMap[Int, Int](0 -> 1)

  for (coin <- coins) {
    for (i <- coin to money) {
      val currentValue = partitions.getOrElse(i, 0)
      val previousValue = partitions.get(i - coin).get
      partitions.put(i, currentValue + previousValue)
    }
  }

  partitions.getOrElse(money, 0)
}

changeFor(200, List(1, 2, 5, 10, 20, 50, 100, 200))
