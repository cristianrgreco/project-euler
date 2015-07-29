import scala.collection.mutable
import scala.io.Source

val filename = "/Users/cristiangreco/Documents/Playground/scala-workspace/src/main/scala/euler/18-input.txt"

val triangle = Source.fromFile(filename).mkString
  .split("\n")
  .map(numString => numString.split(" ").toList.map(_.toInt))
  .map(mutable.ArrayBuffer(_).flatten)

for (i <- (triangle.length - 2).to(0).by(-1)) {
  for (j <- 0.to(i)) {
    triangle(i)(j) = triangle(i)(j) + math.max(triangle(i + 1)(j), triangle(i + 1)(j + 1))
  }
}

triangle.head.head