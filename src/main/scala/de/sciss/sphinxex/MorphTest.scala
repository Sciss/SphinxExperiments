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

import java.awt.{Color, Font}
import java.awt.geom.PathIterator

import de.sciss.numbers
import org.jdesktop.swingx.geom.Morphing2D

import scala.swing.Swing._
import scala.swing.event.{ValueChanged, EditDone}
import scala.swing.{BorderPanel, BoxPanel, Component, Frame, Graphics2D, Orientation, Slider, SwingApplication, TextField}

/*
  Ok, that SwingX class doesn't handle SEG_MOVETO.
  Possible alternatives:
  - http://www.reportmill.com/snap1/javadoc/com/reportmill/shape/RMMorphShape.Morphing2D.html
  - https://gist.github.com/Sciss/d7456f71f2de12fac2e0
  - https://searchcode.com/codesearch/view/64647380/

 */
object MorphTest extends SwingApplication {
  def startup(args: Array[String]): Unit = {
    def mkText(init: String) = new TextField(init, 8) {
      listenTo(this)
      reactions += {
        case EditDone(_) => ggShape.repaint()
      }
    }

    lazy val ggTextA  = mkText("N") // "EINGANGE"
    lazy val ggTextB  = mkText("Z") // "IN GANG"
    lazy val ggSlider = new Slider {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
    }

    lazy val ggShape: Component = new Component {
      preferredSize = (480, 120)

      font        = new Font(Font.SANS_SERIF, Font.PLAIN, 48)
      background  = Color.black
      foreground  = Color.white

      override def paintComponent(g: Graphics2D): Unit = {
        g.setColor(background)
        val w = peer.getWidth
        val h = peer.getHeight
        g.fillRect(0, 0, w, h)
        g.setColor(foreground)

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

        val shpMorph  = new Morphing2D(shpA, shpB)
        import numbers.Implicits._
        val f         = ggSlider.value.linlin(ggSlider.min, ggSlider.max, 0, 1)
        shpMorph.setMorphing(f)
        val shp   = shpMorph
        val r     = shp.getBounds
        g.translate(-r.x + 8, -r.y + 8)
        g.fill(shp) // shpMorph)
      }
    }

    val pBot = new BoxPanel(Orientation.Vertical) {
      contents += ggTextA
      contents += ggTextB
      contents += ggSlider
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
