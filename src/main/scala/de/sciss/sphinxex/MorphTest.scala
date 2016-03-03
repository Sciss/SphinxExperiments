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

import java.awt.geom.{Path2D, PathIterator, AffineTransform}
import java.awt.{Shape, Color, RenderingHints}
import javax.swing.{SpinnerNumberModel, Timer}

import de.sciss.kollflitz
import de.sciss.numbers
import de.sciss.shapeint.ShapeInterpolator
import de.sciss.swingplus.Spinner

import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, EditDone, ValueChanged}
import scala.swing.{Button, BorderPanel, BoxPanel, CheckBox, Component, Frame, Graphics2D, Orientation, Slider, SwingApplication, TextField}

object MorphTest extends SwingApplication {
  private final val PiH = (math.Pi/2).toFloat

  sealed trait PathCmd {
    def addTo(p: Path2D): Unit
  }
  final case class PathMove(x: Double, y: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.moveTo(x, y)
  }
  final case class PathLine(x: Double, y: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.lineTo(x, y)
  }
  final case class PathQuad(x1: Double, y1: Double, x2: Double, y2: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.quadTo(x1, y1, x2, y2)
  }
  final case class PathCube(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.curveTo(x1, y1, x2, y2, x3, y3)
  }
  case object PathClose extends PathCmd {
    def addTo(p: Path2D): Unit = p.closePath()
  }

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
    lazy val mShiftB   = new SpinnerNumberModel(0, 0, 32, 1)
    lazy val ggShiftB  = new Spinner(mShiftB) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
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

            def shiftShape(name: String, shp: Shape, shift: Int): Shape = {
              val it  = shp.getPathIterator(null)
              val vb  = Vector.newBuilder[PathCmd]
              val c   = new Array[Double](6)
              // first create a collection of commands
              while (!it.isDone) {
                import PathIterator._
                val cmd = it.currentSegment(c) match {
                  case SEG_MOVETO  => PathMove(c(0), c(1))
                  case SEG_LINETO  => PathLine(c(0), c(1))
                  case SEG_QUADTO  => PathQuad(c(0), c(1), c(2), c(3))
                  case SEG_CUBICTO => PathCube(c(0), c(1), c(2), c(3), c(4), c(5))
                  case SEG_CLOSE   => PathClose
                }
                vb += cmd
                it.next()
              }
              val v = vb.result()

              import kollflitz.Ops._
              val itGroup = v.groupWith {
                case (PathClose, PathMove(_, _)) => false
                case _ => true
              }
              val head = itGroup.next()

              val segm = head.groupWith {
                case (_, PathLine(_, _)) => false
                case _ => true
              } .toIndexedSeq

              // println("----SEGM----")
              // segm.foreach(println)

              if (shift >= segm.size) println(s"WARNING: $name segm.size = ${segm.size}")

              val iShift1 = shift % segm.size
              val res     = if (iShift1 == 0) shp else {
                val (pre, suf)  = segm.splitAt(iShift1)
                val (PathMove(x0, y0) +: preTail) = pre.flatten
                val (PathLine(x1, y1) +: sufInner :+ PathClose) = suf.flatten
                val p = new Path2D.Float()
                p.moveTo(x1, y1)
                sufInner.foreach(_.addTo(p))
                p.lineTo(x0, y0)
                preTail.foreach(_.addTo(p))
                p.closePath()
                itGroup.foreach { commands =>
                  commands.foreach(_.addTo(p))
                }
                p
              }

              res
            }

            val iShiftA = mShiftA.getNumber.intValue()
            val iShiftB = mShiftB.getNumber.intValue()
            val shpA    = if (iShiftA == 0) shpA0 else shiftShape("A", shpA0, iShiftA)
            val shpB    = if (iShiftB == 0) shpB0 else shiftShape("B", shpB0, iShiftB)

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
      contents += ggShiftA
      contents += ggShiftB
      contents += ggResetShift
      contents += ggPostShift
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