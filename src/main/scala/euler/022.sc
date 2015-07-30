import scala.io.Source

val characterRange = 'A'.to('Z').toList
val characterValues = characterRange.indices.map(i => (characterRange(i), i + 1)).toMap

def fromFile(file: String): List[String] =
  Source.fromFile(file).mkString.replaceAll("\"", "").split(",").toList


val file = "/Users/cristiangreco/Documents/Playground/scala-workspace/src/main/scala/euler/022-input.txt"

val names = fromFile(file).sorted

val scores = names.indices.map(index => {
  val name = names(index)
  val namePoints = name.map(c => characterValues(c)).sum
  namePoints * (index + 1)
})

scores.sum
