def powersFor(a: Range, b: Range): List[BigInt] =
  a.flatMap(i => b.map(j => BigInt(i).pow(j))).toList


val aRange = 2.to(100)
val bRange = 2.to(100)

powersFor(aRange, bRange).distinct.length
