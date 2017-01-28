/*
 *  MorphTest.scala
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

import java.awt.geom.AffineTransform
import java.awt.{Color, RenderingHints}
import javax.swing.{SpinnerNumberModel, Timer}

import de.sciss.numbers
import de.sciss.shapeint.ShapeInterpolator
import de.sciss.sphinxex.sikring.Vertex
import de.sciss.swingplus.Spinner

import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, EditDone, ValueChanged}
import scala.swing.{BorderPanel, BoxPanel, Button, CheckBox, Component, FlowPanel, Frame, Graphics2D, Orientation, Slider, SwingApplication, TextField}

object MorphTest extends SwingApplication {
  private final val PiH = (math.Pi/2).toFloat

  def startup(args: Array[String]): Unit = {
    var timer     = Option.empty[Timer]
    var timerStep = 1.0f / 30

    def mkText(init: String) = new TextField(init, 8) {
      listenTo(this)
      reactions += {
        case EditDone(_) => ggShape.repaint()
      }
    }

    lazy val ggTextA  = mkText("D") // "BEIM NACHDENKEN ÜBER" // "EINGANGE"
    lazy val ggTextB  = mkText("O") // "BEI NACHT IN ÜBER" // "IN GANG"
    lazy val ggSlider = new Slider {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
    }
    lazy val mShiftA   = new SpinnerNumberModel(0, 0, 32, 1)
    lazy val ggShiftA  = new Spinner(mShiftA) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
    }
    lazy val ggRevA    = new CheckBox("Reverse") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => ggShape.repaint()
      }
    }
    lazy val mShiftB   = new SpinnerNumberModel(0, 0, 32, 1)
    lazy val ggShiftB  = new Spinner(mShiftB) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
    }
    lazy val ggRevB    = new CheckBox("Reverse") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => ggShape.repaint()
      }
    }
    lazy val ggResetShift = Button("Reset") {
      mShiftA.setValue(0)
      mShiftB.setValue(0)
    }
    lazy val ggPostShift = Button("Post") {
      val a   = ggTextA.text.head
      val b   = ggTextB.text.head
      val sa  = mShiftA.getNumber.intValue()
      val sb  = mShiftB.getNumber.intValue()
      println(s""""$a$b": [$sa, $sb],""") // json style
    }

    import numbers.Implicits._

    def fraction: Float =
      ggSlider.value.linlin(ggSlider.min, ggSlider.max, 0, 1)

    def fraction_=(value: Float): Unit =
      ggSlider.value = (value.linlin(0, 1, ggSlider.min, ggSlider.max) + 0.5).toInt

    lazy val ggTimer = new CheckBox("Animate") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          timer.foreach(_.stop())
          if (selected) {
            val t = new Timer(30, ActionListener { _ =>
              val f1 = fraction + timerStep
              if (f1 < 0 || f1 > 1) {
                timerStep = -timerStep
              }
              fraction_=(f1.clip(0, 1))
              toolkit.sync()  // Linux bug
            })
            t.setRepeats(true)
            timer = Some(t)
            t.start()
          } else {
            timer = None
          }
      }
    }

    lazy val ggShape: Component = new Component {
      preferredSize = (480, 120)

      font        = MyFont(64)
      background  = Color.black
      foreground  = Color.white

      override def paintComponent(g: Graphics2D): Unit = {
        g.setColor(background)
        val w = peer.getWidth
        val h = peer.getHeight
        g.fillRect(0, 0, w, h)
        g.setColor(foreground)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )

        val frc       = g.getFontRenderContext
        val font      = g.getFont
        val txtA      = ggTextA.text
        val txtB      = ggTextB.text
        val vecA      = font.createGlyphVector(frc, txtA)
        val vecB      = font.createGlyphVector(frc, txtB)
        val trans     = EditTranscript(txtA, txtB)
        val numT      = trans.length

        val rx    =   0.0 // r.getMinX //  0.0 // math.min(r.getMinX, r.getMaxX)
        val ry    = -56.0 // r.getMinY // -36.0 // math.min(r.getMinY, r.getMaxY)
        g.translate(-rx + 8, -ry + 8)

        val shpMorph  = new ShapeInterpolator
        val f         = fraction.linlin(0, 1, -PiH, PiH).sin.linlin(-1, 1, 0, 1)
        var aIdx      = 0
        var bIdx      = 0
        var tIdx      = 0
        while (tIdx < numT) {
          import EditTranscript._
          val edit = trans.charAt(tIdx)
          if (edit == Copy) {
            val shpA  = vecA.getGlyphOutline(aIdx)
            val posA  = vecA.getGlyphPosition(aIdx).getX
            val posB  = vecB.getGlyphPosition(bIdx).getX
            val shp   = if (f == 0) shpA else {
              val dx = f.linlin(0, 1, 0, posB - posA)
              val at = AffineTransform.getTranslateInstance(dx, 0)
              at.createTransformedShape(shpA)
            }
            g.fill(shp)

            // Note: treat S(' ', _) as I and treat S(_, ' ') as D
          } else if (edit == Substitute && txtA.charAt(aIdx) != ' ' && txtB.charAt(bIdx) != ' ') {
            val shpA0 = vecA.getGlyphOutline(aIdx)
            val shpB0 = vecB.getGlyphOutline(bIdx)

            val iShiftA = mShiftA.getNumber.intValue()
            val iShiftB = mShiftB.getNumber.intValue()
            val shpA1   = if (!ggRevA.selected) shpA0 else Vertex.reverseShape(shpA0)
            val shpB1   = if (!ggRevB.selected) shpB0 else Vertex.reverseShape(shpB0)
            val shpA    = if (iShiftA == 0) shpA1 else Vertex.shiftShape("A", shpA1, iShiftA)
            val shpB    = if (iShiftB == 0) shpB1 else Vertex.shiftShape("B", shpB1, iShiftB)

            val shp   = shpMorph.evaluate(shpA, shpB, f, true)
            g.fill(shp)

          } else if (edit == Insert || (edit == Substitute && txtA.charAt(aIdx) == ' ')) {
            if (f > 0) {
              val shpB  = vecB.getGlyphOutline(bIdx)
              val shp   = if (f == 1) shpB else {
                val r     = shpB.getBounds2D
                val scale = f
                val at    = AffineTransform.getTranslateInstance(r.getCenterX, r.getCenterY)
                at.scale(scale, scale)
                at.translate(-r.getCenterX, -r.getCenterY)
                at.createTransformedShape(shpB)
              }
              g.fill(shp)
            }

          } else /* Delete */ {
            if (f < 1) {
              val shpA  = vecA.getGlyphOutline(aIdx)
              val shp   = if (f == 0) shpA else {
                val r     = shpA.getBounds2D
                val scale = 1 - f
                val at    = AffineTransform.getTranslateInstance(r.getCenterX, r.getCenterY)
                at.scale(scale, scale)
                at.translate(-r.getCenterX, -r.getCenterY)
                at.createTransformedShape(shpA)
              }
              g.fill(shp)
            }
          }

          tIdx += 1
          if (edit != Insert) aIdx += 1
          if (edit != Delete) bIdx += 1
        }
      }
    }

    val pBot = new BoxPanel(Orientation.Vertical) {
      contents += ggTextA
      contents += ggTextB
      contents += ggSlider
      contents += new FlowPanel(ggShiftA, ggRevA)
      contents += new FlowPanel(ggShiftB, ggRevB)
      contents += new FlowPanel(ggResetShift, ggPostShift)
      contents += ggTimer
    }

    new Frame {
      title = "Morph Test"
      contents = new BorderPanel {
        add(ggShape, BorderPanel.Position.Center)
        add(pBot   , BorderPanel.Position.South )
      }

      override def closeOperation(): Unit = sys.exit()

      pack().centerOnScreen()
      open()
    }
  }
}