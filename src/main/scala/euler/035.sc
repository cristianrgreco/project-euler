import scala.collection.parallel.mutable
import scala.language.postfixOps

def circularPrimesInRange(r: Range): Seq[Seq[Int]] = {
  def rotationsFor(n: Int): Seq[Int] = {
    val s = n.toString
    for (i <- s.length until 0 by -1 toList)
      yield (s.drop(i) + s.take(i)).toInt
  }

  def sieve(limit: Int) = {
    val primes: mutable.ParSet[Int] = mutable.ParSet.empty ++ (2 to limit)
    val sqrtLimit = math.sqrt(limit).toInt
    def prim(candidate: Int): Unit = {
      if (candidate <= sqrtLimit) {
        if (primes contains candidate) primes --= candidate * candidate to limit by candidate
        prim(candidate + 1)
      }
    }
    prim(2)
    primes
  }

  val rotations = r.map(rotationsFor)
  val primes = sieve(rotations.flatten.max)

  rotations.filter(l => l.forall(i => primes.contains(i))).toSeq
}

circularPrimesInRange(2 until 1000000).length
