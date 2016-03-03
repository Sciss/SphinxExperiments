/*
 *  Renderer.scala
 *  (SphinxExperiments)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.sphinxex
package sikring

import java.awt.Shape
import java.awt.image.BufferedImage

import de.sciss.kollflitz

import scala.collection._
import scala.concurrent.stm._

object Renderer {
  def apply(phrases: Vec[Vec[String]]): Graph = {
    /*
        ---- edge forces ----

        say we delete a character:

        "BAR"
        "B*R"

        thus we need edges (B, A), (A, R) and (B, R)

        let's look at a more complex example:

        "f**rom**"
        "c*hrome*"
        "c*as**e*"
        "plac**e*"
        "i**c**e*"
        "i**nn*er"

        - each column is a vertex
        - thus we can connect each pair of adjacent columns
        - we look from each column to all its right neighbours
        - if a neighbour column contains a '*' in any row
          for which the current column does not contain a '*'
          in that same row, and the n-th neighbour column
          does not contain a '*' in that row, we add another
          edge, where n = 2 to width

        // note -- we could abort as soon as `b` is false.
        // if we start with n = 2, we automatically include all adjacent vertices
        for (n <- 3 to (numCols - colIdx)) {
          val b = (0 until numRows).exists { rowIdx =>
            val sel = columns.slice(colIdx, colIdx + n)
            val sub = sel.map(_.apply(rowIdx)).mkString
            sub.head.isDefined && sub.last.isDefined &&
              sub.init.tail.forall(_.isEmpty)
          }
          if (b) addEdge
        }
     */

    val graph   = Graph()
    val font    = MyFont(64)

    val tmpImg  = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val tmpG    = tmpImg.createGraphics()
    val fm      = tmpG.getFontMetrics(font)
    val frc     = fm.getFontRenderContext

    import kollflitz.Ops._

    val phrasesFlat = phrases.flatten
    val charSet   : Set[Char]         = phrasesFlat.flatMap(x => x: Vec[Char])(breakOut)
    val charShapes: Map[Char, Shape]  = charSet.map { c =>
      val gv    = font.createGlyphVector(frc, c.toString)
      val shape = gv.getOutline
      c -> shape
    } (breakOut)

    val charPairs: Set[(Char, Char)]  = phrasesFlat.flatMap { ph =>
      (ph: Vec[Char]).mapPairs((a, b) => (a, b))(breakOut): Set[(Char, Char)]
    } (breakOut)

    val charPairSpacing: Map[(Char, Char), Double] = charPairs.map { case pair @ (a, b) =>
      val gv    = font.createGlyphVector(frc, s"$a$b")
      //      val shpA    = gv.getGlyphOutline(0)
      //      val shpB    = gv.getGlyphOutline(1)
      //      val rA      = shpA.getBounds2D
      //      val rB      = shpB.getBounds2D
      //      val dist    = rB.getCenterX - rA.getCenterX
      val pA    = gv.getGlyphPosition(0)
      val pB    = gv.getGlyphPosition(1)
      val dist  = pB.getX - pA.getX
      pair -> dist
    } (breakOut)

    tmpG.dispose()

    val phraseShapes: Vec[Vec[Vec[(Char, Shape)]]] = phrases.map { phraseSet =>
      phraseSet.map { phrase =>
        phrase.map(c => (c, charShapes(c)))
      }
    }

    val alignedAll  = phraseShapes.map { shapesSet  => EditTranscript.alignWith(shapesSet , fill = Vertex.EmptyShape) }
    val alignedAllS = phrases     .map { phrasesSet => EditTranscript.align    (phrasesSet, fill = '*') }

    // println(alignedS.mkString("\n"))

    val PhasePeriod = 4 * 25

    atomic { implicit tx =>
      (alignedAll zip alignedAllS).zipWithIndex.foreach { case ((aligned, alignedS), lineIdx) =>
          processPhrase(aligned, alignedS, lineIdx = lineIdx)
      }
    }

    def processPhrase(aligned: Vec[Vec[Shape]], alignedS: Vec[String], lineIdx: Int)(implicit tx: InTxn): Unit = {
      // create vertices
      val columns     = aligned .transpose
      val columnsS    = alignedS.transpose
      val numRows     = aligned.size
      val numColumns  = columns.size

      val yPos      = lineIdx * 84 + 64

      val vertices = (columns zip columnsS).zipWithIndex.map { case ((columnShapes, columnNames), columnIdx) =>
        val seq     = columnNames zip columnShapes
        val v       = Vertex(columnIdx.toString, startTime = 0, phasePeriod = PhasePeriod, seq = seq)
        v.position  = DoublePoint2D(x = columnIdx * 48 + 8, y = yPos)
        graph.addVertex(v)
        v
      }

      // create edges
      for (colIdx <- 0 until (numColumns - 1)) {
        for (n <- 2 /* 3 */ to (numColumns - colIdx)) {
          val b = (0 until numRows).exists { rowIdx =>
            val sel = columns.slice(colIdx, colIdx + n)
            val sub = sel.map(_.apply(rowIdx)) // .mkString
          val res = sub.head.isDefined && sub.last.isDefined && sub.init.tail.forall(_.isEmpty)
            // if (res) println(sub.mkString)
            res
          }
          if (b) {
            val sourceIdx = colIdx
            val sinkIdx   = colIdx + n - 1
            val sourceV   = vertices(sourceIdx)
            val sinkV     = vertices(sinkIdx)
            val spacing   = (aligned zip alignedS).map { case (sub, row) =>
              val active  = sub(sourceIdx).isDefined && sub(sinkIdx).isDefined &&
                sub.slice(sourceIdx + 1, sinkIdx).forall(_.isEmpty)
              if (!active) 0.0 else {
                val pair = (row(sourceIdx), row(sinkIdx))
                charPairSpacing(pair) // .getOrElse(pair, 0.0)
              }
            }
            println(s"$colIdx > ${colIdx + n - 1} : ${spacing.mkString(", ")}")
            val force     = Force.HTorque(startTime = 0, phasePeriod = PhasePeriod, seq = spacing)
            val e         = Edge(sourceV, sinkV, force = force)
            graph.addEdge(e)
          }
        }
      }
    }

    graph
  }
}
