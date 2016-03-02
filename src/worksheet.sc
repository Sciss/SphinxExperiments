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

EditTranscript.align(in).mkString("\n","\n",""  )
/*
        "f**r*om*"
        "c*hr*ome"
        "c*as*e**"
        "plac*e**"
        "i**c*e**"
        "i**nner*"
 */