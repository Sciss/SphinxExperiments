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
import de.sciss.sphinxex.sikring.{DoublePoint2D, Graph, Vertex}
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

    val charSet     = phrases.flatMap(x => x: Vec[Char])(breakOut): Set[Char]
    val charShapes  = charSet.map { c =>
      val gv    = font.createGlyphVector(frc, c.toString)
      val shape = gv.getOutline
      c -> shape
    } (breakOut): Map[Char, Shape]

    val charPairs   = phrases.flatMap { ph =>
      (ph: Vec[Char]).mapPairs((a, b) => (a, b))(breakOut): Set[(Char, Char)]
    } (breakOut): Set[(Char, Char)]

    val charPairSpacing = charPairs.map { case pair @ (a, b) =>
      val gv      = font.createGlyphVector(frc, s"$a$b")
      val shpA    = gv.getGlyphOutline(0)
      val shpB    = gv.getGlyphOutline(1)
      val rA      = shpA.getBounds2D
      val rB      = shpB.getBounds2D
      val dist    = rB.getCenterX - rA.getCenterX
      pair -> dist
    } (breakOut): Map[(Char, Char), Double]

    tmpG.dispose()

    val phraseShapes: Vec[Vec[(Char, Shape)]] = phrases.map { phrase =>
      phrase.map(c => (c, charShapes(c)))
    }

    val aligned = EditTranscript.alignWith(phraseShapes, fill = Vertex.EmptyShape)

    val PhasePeriod = 4 * 25

    atomic { implicit tx =>
      aligned.transpose.zipWithIndex.foreach { case (columnShapes, columnIdx) =>
        val v       = Vertex(startTime = 0, phasePeriod = PhasePeriod, seq = columnShapes)
        v.position  = DoublePoint2D(x = columnIdx * 48 + 8, y = 64)
        graph.addVertex(v)
      }
    }

    def tickAction(): Unit = {
      atomic { implicit tx => graph.tick() }
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

    val pBut  = new FlowPanel(ggTick, new Label("fps:"), ggFPS, ggAnim)

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
