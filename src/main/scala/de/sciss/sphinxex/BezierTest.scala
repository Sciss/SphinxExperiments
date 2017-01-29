package de.sciss.sphinxex

import java.awt.{Color, RenderingHints}
import java.awt.geom.{Ellipse2D, Path2D}

import de.sciss.sphinxex.sikring.impl.VertexImpl.{PathCmdTo, PathCube, PathMove}

import scala.swing.{Component, Frame, Graphics2D, MainFrame, SimpleSwingApplication}
import scala.swing.Swing._

object BezierTest extends SimpleSwingApplication {
  lazy val top: Frame = new MainFrame {
    contents = new Component {
      preferredSize = (400, 400)

      val start = PathMove(10, 200)
      val end   = PathMove(390, 200)
      val ctl1  = PathMove(10, 10)
      val ctl2  = PathMove(390, 390)
      val cube  = PathCube(ctl1.x, ctl1.y, ctl2.x, ctl2.y, end.x, end.y)
      val p     = new Path2D.Double()
      start.addTo(p)
      cube .addTo(p)

      val ell   = new Ellipse2D.Double

      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)

        def drawPt(pt: PathCmdTo, fill: Color): Unit = {
          ell.setFrameFromCenter(pt.endX, pt.endY, pt.endX + 4, pt.endY + 4)
          g.setColor(fill)
          g.fill(ell)
          g.setColor(Color.black)
          g.draw(ell)
        }

        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
        g.setColor(Color.black)
        g.draw(p)
        drawPt(start, Color.lightGray)
        drawPt(ctl1 , Color.blue)
        drawPt(ctl2 , Color.blue)
        drawPt(end  , Color.lightGray)

        val i = cube.toLines(start.x, start.y, 10)
        i.dropRight(1).foreach { ln => drawPt(ln, Color.red) }
      }
    }
  }
}