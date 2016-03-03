/*
 *  SikringTest.scala
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

import java.awt.image.BufferedImage
import java.awt.{RenderingHints, Color, Shape}
import javax.swing.{Timer, SpinnerNumberModel}

import de.sciss.kollflitz
import de.sciss.sphinxex.sikring.{Force, Edge, DoublePoint2D, Graph, Vertex}
import de.sciss.swingplus.Spinner

import scala.collection.breakOut
import scala.concurrent.stm.atomic
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{Swing, Label, CheckBox, BorderPanel, FlowPanel, Button, Dimension, MainFrame, Graphics2D, Component, Frame, SimpleSwingApplication}

object SikringTest extends SimpleSwingApplication {
  lazy val top: Frame = {
    val phrases = Vec(
      "DIE JUWELEN ES",
      "DIE VIREN ES",
      "JUWELEN ES",
      "THEORIEN ES")

//    val phrases = Vec("D", "")

//    val phrases = Vec("DAS", "DS")

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

    val graph = Graph()
    val font  = MyFont(64)
    val comp  = new Component {
      background    = Color.black
      foreground    = Color.white
      preferredSize = new Dimension(800, 84)
      opaque        = true

      override protected def paintComponent(g: Graphics2D): Unit = {
        g.setColor(background)
        g.fillRect(0, 0, peer.getWidth, peer.getHeight)
        g.setColor(foreground)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
        atomic { implicit tx =>
          graph.render(g)
        }
      }
    }
    val tmpImg  = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val tmpG    = tmpImg.createGraphics()
    val fm      = tmpG.getFontMetrics(font)
    val frc     = fm.getFontRenderContext

    import kollflitz.Ops._

    val charSet   : Set[Char]         = phrases.flatMap(x => x: Vec[Char])(breakOut)
    val charShapes: Map[Char, Shape]  = charSet.map { c =>
      val gv    = font.createGlyphVector(frc, c.toString)
      val shape = gv.getOutline
      c -> shape
    } (breakOut)

    val charPairs: Set[(Char, Char)]  = phrases.flatMap { ph =>
      (ph: Vec[Char]).mapPairs((a, b) => (a, b))(breakOut): Set[(Char, Char)]
    } (breakOut)

    val charPairSpacing: Map[(Char, Char), Double] = charPairs.map { case pair @ (a, b) =>
      val gv      = font.createGlyphVector(frc, s"$a$b")
      val shpA    = gv.getGlyphOutline(0)
      val shpB    = gv.getGlyphOutline(1)
      val rA      = shpA.getBounds2D
      val rB      = shpB.getBounds2D
      val dist    = rB.getCenterX - rA.getCenterX
      pair -> dist
    } (breakOut)

    tmpG.dispose()

    val phraseShapes: Vec[Vec[(Char, Shape)]] = phrases.map { phrase =>
      phrase.map(c => (c, charShapes(c)))
    }

    val aligned   = EditTranscript.alignWith(phraseShapes, fill = Vertex.EmptyShape)
    val alignedS  = EditTranscript.align    (phrases     , fill = '_')

    val PhasePeriod = 4 * 25

    atomic { implicit tx =>
      // create vertices
      val columns     = aligned.transpose
      val numRows     = aligned.size
      val numColumns  = columns.size

      val vertices = columns.zipWithIndex.map { case (columnShapes, columnIdx) =>
        val v       = Vertex(columnIdx.toString, startTime = 0, phasePeriod = PhasePeriod, seq = columnShapes)
        v.position  = DoublePoint2D(x = columnIdx * 48 + 8, y = 64)
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

    val ggAdvance = new CheckBox("Advance") {
      selected = true
    }

    def tickAction(): Unit = {
      val advance = ggAdvance.selected
      atomic { implicit tx => graph.tick(advance = advance) }
      comp.repaint()
    }

    val ggTick  = Button("Tick")(tickAction())
    val timer   = new Timer(1000/25, Swing.ActionListener(_ => tickAction()))

    val mFPS  = new SpinnerNumberModel(25, 1, 60, 1)
    val ggFPS = new Spinner(mFPS) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          val millis = 1000 / mFPS.getNumber.intValue()
          timer.setInitialDelay(millis)
          timer.setDelay       (millis)
      }
    }

    val ggAnim = new CheckBox("Anim") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          if (selected) timer.restart() else timer.stop()
      }
    }

    val pBut  = new FlowPanel(ggTick, ggAdvance, new Label("fps:"), ggFPS, ggAnim)

    new MainFrame {
      title     = "Sikring Test"
      contents  = new BorderPanel {
        add(comp, BorderPanel.Position.Center)
        add(pBut, BorderPanel.Position.South )
      }
      pack().centerOnScreen()

      override def closeOperation(): Unit = {
        timer.stop()
        super.closeOperation()
      }
    }
  }
}
