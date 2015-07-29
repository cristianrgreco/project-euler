import scala.collection.mutable
import scala.io.Source

val filename = "/Users/cristiangreco/Documents/Playground/scala-workspace/src/main/scala/euler/18-input.txt"

val triangle = Source.fromFile(filename).mkString
  .split("\n")
  .map(numString => numString.split(" ").toList.map(_.toInt))
  .map(mutable.ArrayBuffer(_).flatten)

//val triangle = mutable.ArrayBuffer(
//  mutable.ArrayBuffer(3),
//  mutable.ArrayBuffer(7, 4),
//  mutable.ArrayBuffer(2, 4, 6),
//  mutable.ArrayBuffer(8, 5, 9, 3)
//)
//val a = triangle(2)(0) + math.max(triangle(3)(0), triangle(3)(1))
//val b = triangle(2)(1) + math.max(triangle(3)(1), triangle(3)(2))
//val c = triangle(2)(2) + math.max(triangle(3)(2), triangle(3)(3))
//
//val d = triangle(1)(0) + math.max(a, b)
//val e = triangle(1)(1) + math.max(b, c)
//
//val ans = triangle(0)(0) + math.max(d, e)
//
//
for (i <- (triangle.length - 2).to(0).by(-1)) {
  for (j <- 0.to(i)) {
//     println(s"triangle($i)($j) = triangle($i)($j) + math.max(triangle(${i+1})($j), triangle(${i+1})(${j+1}))")
    triangle(i)(j) = triangle(i)(j) + math.max(triangle(i + 1)(j), triangle(i + 1)(j + 1))
  }
}

triangle.head.head