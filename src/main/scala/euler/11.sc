import scala.io.Source

def largestProduct(file: String, width: Int): BigInt = {
  // access: grid(row)(column)
  val grid = Source.fromFile(file).getLines().map(line => line.split(" ").map(s => s.toInt).toVector).toVector

  def getAdjacentFor(gridPos: (Int, Int)): Vector[Vector[Int]] = {
    val (y, x) = gridPos

    val bottom =
      if (y + width > grid.length) Vector()
      else y.until(y + width).map(i => grid(i)(x)).toVector

    val left =
      if (x - width < 0) Vector()
      else x.until(x - width).by(-1).map(i => grid(y)(i)).toVector

    val right =
      if (x + width > grid.length) Vector()
      else x.until(x + width).map(i => grid(y)(i)).toVector

    val diagBottomLeft =
      if (x - width < 0 || y + width > grid.length) Vector()
      else (for (i <- 0 until width) yield i).map(i => grid(y + i)(x - i)).toVector

    val diagBottomRight =
      if (x + width > grid.length || y + width > grid.length) Vector()
      else (for (i <- 0 until width) yield i).map(i => grid(y + i)(x + i)).toVector

    Vector(bottom, left, right, diagBottomLeft, diagBottomRight)
  }

  val indices = 0.until(grid.length).flatMap(rowIndex => 0.until(grid.length).map(colIndex => (rowIndex, colIndex)))

  val adjacents = indices.flatMap(getAdjacentFor)
  val products = adjacents.map(v => v.product)

  products.max
}

val gridFile = "/Users/cristiangreco/Documents/Playground/scala-workspace/src/main/scala/euler/11-input.txt"
val width = 4

largestProduct(gridFile, width)
