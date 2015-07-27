def sumOfSquares(r: Range): Int = {
  (for {
    n <- r
    x = n * n
  } yield x).sum
}

def squareOfSums(r: Range): Int = {
  math.pow((for (n <- r) yield n).sum, 2).toInt
}

squareOfSums(1 to 100) - sumOfSquares(1 to 100)
