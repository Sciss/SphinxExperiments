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

import java.awt.{Color, RenderingHints}
import javax.swing.Timer

import de.sciss.numbers
import de.sciss.shapeint.ShapeInterpolator

import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, EditDone, ValueChanged}
import scala.swing.{BorderPanel, BoxPanel, CheckBox, Component, Frame, Graphics2D, Orientation, Slider, SwingApplication, TextField}

/*
  Ok, that SwingX class doesn't handle SEG_MOVETO.
  Possible alternatives:
  - http://www.reportmill.com/snap1/javadoc/com/reportmill/shape/RMMorphShape.Morphing2D.html
    (proprietary, seems they just stole the LGPL'ed SwingX code)
  - https://gist.github.com/Sciss/d7456f71f2de12fac2e0
  - https://searchcode.com/codesearch/view/64647380/

 */
object MorphTest extends SwingApplication {
  def startup(args: Array[String]): Unit = {
    var timer = Option.empty[Timer]
    var timerStep = 1.0f / 30

    def mkText(init: String) = new TextField(init, 8) {
      listenTo(this)
      reactions += {
        case EditDone(_) => ggShape.repaint()
      }
    }

//    lazy val ggTextA  = mkText("N")
//    lazy val ggTextB  = mkText("Z")
    lazy val ggTextA  = mkText("EINGANGE")
    lazy val ggTextB  = mkText("IN GANG" )
    lazy val ggSlider = new Slider {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
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

      font        = MyFont(64) // new Font(Font.SANS_SERIF, Font.PLAIN, 64)
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
        val vecA      = font.createGlyphVector(frc, ggTextA.text)
        val vecB      = font.createGlyphVector(frc, ggTextB.text)
        val shpA      = vecA.getOutline
        val shpB      = vecB.getOutline

//        val itA = shpA.getPathIterator(g.getTransform)
//        val arr = new Array[Float](6)
//        while (!itA.isDone) {
//          val tpe  = itA.currentSegment(arr)
//          val name = tpe match {
//            case PathIterator.SEG_MOVETO => "move-to"
//            case PathIterator.SEG_LINETO => "line-to"
//            case PathIterator.SEG_QUADTO => "quad-to"
//            case PathIterator.SEG_CUBICTO=> "cube-to"
//            case PathIterator.SEG_CLOSE  => "close"
//          }
//          println(name)
//          itA.next()
//        }

        // val shpMorph  = new Morphing2D(shpA, shpB)
        val shpMorph  = new ShapeInterpolator
        val f         = fraction
        // shpMorph.setMorphing(f)
        // val shp   = shpMorph
        val shp   = shpMorph.evaluate(shpA, shpB, f, true)
        val r     = shp.getBounds
        val rx    = r.getMinX //  0.0 // math.min(r.getMinX, r.getMaxX)
        val ry    = r.getMinY // -36.0 // math.min(r.getMinY, r.getMaxY)
        g.translate(-rx + 8, -ry + 8)
        g.fill(shp) // shpMorph)
      }
    }

    val pBot = new BoxPanel(Orientation.Vertical) {
      contents += ggTextA
      contents += ggTextB
      contents += ggSlider
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
