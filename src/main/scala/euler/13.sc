import scala.io.Source

def readFileByChunksOf(file: String, chunks: Int): List[BigInt] =
  Source.fromFile(file).mkString.replaceAll("[\\s]", "").grouped(chunks).toList.map(BigInt(_))

val filename = "/Users/cristiangreco/Documents/Playground/scala-workspace/src/main/scala/euler/13-input.txt"
val input = readFileByChunksOf(filename, 50)

input.sum.toString().substring(0, 10)
