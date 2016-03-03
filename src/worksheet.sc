import de.sciss.sphinxex.EditTranscript

EditTranscript("ident", "ident")

EditTranscript("vintner", "writers")

EditTranscript("intention", "execution")

EditTranscript(
  "FOLGEN VON HÄUSERN",
  "VORWURF VON HÄUSERN")

val in = Vector(
  "from",
  "chrome",
  "case",
  "place",
  "ice",
  "inner"
)

val t = EditTranscript.align(in)
t.mkString("\n","\n",""  )
/*
        "f**r*om*"
        "c*hr*ome"
        "c*as*e**"
        "plac*e**"
        "i**c*e**"
        "i**nner*"
 */

val columns     = t.map(x => x: Seq[Char]).transpose
val numRows     = t.size
val numColumns  = columns.size

Nil.forall(_ => true)

for (colIdx <- 0 until (numColumns - 1)) {
  for (n <- 3 to (numColumns - colIdx)) {
    val b = (0 until numRows).exists { rowIdx =>
      val sel = columns.slice(colIdx, colIdx + n)
      val sub = sel.map(_.apply(rowIdx)).mkString
      val res = sub.head != '*' && sub.last != '*' &&
        sub.init.tail.forall(_ == '*')
      // if (res) println(sub.mkString)
      res
    }
    if (b) println(s"$colIdx > ${colIdx + n - 1}")
  }
}

/*
  "f**rom**"
  "c*hrome*"
  "c*as**e*"
  "plac**e*"
  "i**c**e*"
  "i**nn*er"

   0 > 2
   0 > 3
   3 > 6
   4 > 6
 */

