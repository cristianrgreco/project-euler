import scala.io.Source

def fromFile(file: String): List[Int] =
  Source.fromFile(file).mkString.replaceAll("[\\s]", "").map(_.asDigit).toList

val filename =
  "/Users/cristiangreco/Documents/Playground/scala-workspace/src/main/scala/euler/8a-input.txt"
val subarraySize = 13

val table = fromFile(filename)

val sizes = (for (i <- 0 to table.length) yield i).toList

val subarrays = sizes
  .map(n => table.slice(n, n + subarraySize))
  .filter(subarray => subarray.length == subarraySize)

val subarraysBigInts =
  subarrays.map(subarray => subarray.map(number => BigInt(number)))

val products = subarraysBigInts.map(subarray => subarray.product)

val largestProduct = products.max
