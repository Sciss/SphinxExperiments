/*
 *  MorphTest.scala
 *  (SphinxExperiments)
 *
 *  Copyright (c) 2016-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.sphinxex

import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.{Color, Font, GraphicsEnvironment, RenderingHints}

import de.sciss.desktop.FileDialog
import de.sciss.file._
import de.sciss.numbers
import de.sciss.shapeint.ShapeInterpolator
import de.sciss.sphinxex.sikring.Vertex
import de.sciss.swingplus.{ComboBox, Spinner}
import javax.imageio.ImageIO
import javax.swing.{SpinnerNumberModel, Timer}

import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, EditDone, SelectionChanged, ValueChanged}
import scala.swing.{Action, BorderPanel, BoxPanel, Button, CheckBox, Component, FlowPanel, Frame, Graphics2D, Label, Orientation, Slider, SwingApplication, TextField, ToggleButton}

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

    def text0 = "Algorithms that Matter"

    lazy val ggTextA  = mkText(text0.toLowerCase()) // "BEIM NACHDENKEN ÜBER" // "EINGANGE"
    lazy val ggTextB  = mkText(text0.toUpperCase()) // "BEI NACHT IN ÜBER" // "IN GANG"

    lazy val ggCopyText = new Button(Action("\\/") {
      ggTextB.text = ggTextA.text.toUpperCase()
      ggShape.repaint()
    }) {
      tooltip = "Copy upper text to lower text and uppercase"
    }

    lazy val ggSlider = new Slider {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          ggShape.repaint()
          lbSlider.text = value.toString
      }
      value   = 20
      tooltip = "Interpolation position"
    }
    lazy val lbSlider = new Label {
      text = "999"
      preferredSize = preferredSize
      text = "50"
    }

    lazy val mShiftA   = new SpinnerNumberModel(0, 0, 256, 1)
    lazy val ggShiftA  = new Spinner(mShiftA) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
      tooltip = "Path-iterator 1 index shift"
    }
    lazy val ggRevA    = new CheckBox("Reverse") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => ggShape.repaint()
      }
      tooltip = "Path-iterator 1 reverse order"
    }
    lazy val mShiftB   = new SpinnerNumberModel(0, 0, 256, 1)
    lazy val ggShiftB  = new Spinner(mShiftB) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => ggShape.repaint()
      }
      tooltip = "Path-iterator 2 index shift"
    }
    lazy val ggRevB    = new CheckBox("Reverse") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => ggShape.repaint()
      }
      tooltip = "Path-iterator 2 reverse order"
    }
    lazy val ggResetShift = new Button(Action("Reset") {
      mShiftA.setValue(0)
      mShiftB.setValue(0)
    }) {
      tooltip = "Reset path-iterator index shifts"
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
      ggSlider.value.linLin(ggSlider.min, ggSlider.max, 0, 1)

    def fraction_=(value: Float): Unit =
      ggSlider.value = (value.linLin(0, 1, ggSlider.min, ggSlider.max) + 0.5).toInt

    lazy val ggInvert = new ToggleButton {
      action = Action("Invert") {
        ggShape.background = if (selected) Color.black else Color.white
        ggShape.foreground = if (selected) Color.white else Color.black
      }
      tooltip = "Invert colors"
    }

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

    def mkExport(name: String, ext: String)(body: File => Unit): Component = {
      var init = userHome / "Documents" / s"glyph.$ext"
      Button(s"Export $name…") {
        val dlg = FileDialog.save(Some(init), title = s"Export as $name")
        dlg.show(None).foreach { f =>
          init = f
          body(f)
        }
      }
    }

    lazy val ggExportPDF = mkExport("PDF", "pdf") { f =>
      val v = ggShape
      de.sciss.pdflitz.Generate(file = f, view = v, usePreferredSize = false, overwrite = true)
    }

    lazy val ggExportPNG = mkExport("PNG", "png") { f =>
      val v = ggShape
      val img = new BufferedImage(v.peer.getWidth, v.peer.getHeight, BufferedImage.TYPE_BYTE_GRAY)
      val g = img.createGraphics()
      v.paint(g)
      g.dispose()
      ImageIO.write(img, "png", f)
    }

    lazy val fontNames: Seq[String] = GulimFont.name +: OpenSansFont.name +: GraphicsEnvironment.getLocalGraphicsEnvironment
      .getAvailableFontFamilyNames.sorted

    lazy val ggFont = new ComboBox(fontNames) {
      listenTo(selection)
      reactions += {
        case SelectionChanged(_) =>
          updateFont()
      }
    }

    def updateFont(): Unit = {
      val name = ggFont.selection.item
      val size = mFontSize.getNumber.intValue()
      ggShape.font =
        if (name == OpenSansFont.name ) OpenSansFont(size.toFloat) else
        if (name == GulimFont.name    ) GulimFont   (size.toFloat)
        else                            new Font    (name, Font.PLAIN, size)
    }

    lazy val ggEvenOdd = new CheckBox("Even-odd") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => ggShape.repaint()
      }
    }

    lazy val ggDiff = new CheckBox("Diff") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => ggShape.repaint()
      }
    }

    lazy val mFontSize = new SpinnerNumberModel(64, 4, 256, 1)
    lazy val ggFontSize = new Spinner(mFontSize) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => updateFont()
      }
      tooltip = "Font Size"
    }

    lazy val mLines = new SpinnerNumberModel(1 /* 0 */, 0, 256, 1)
    lazy val ggLines = new Spinner(mLines) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) => updateFont()
      }
      tooltip = "Convert segments to no. of straight lines"
    }

    lazy val ggShape: Component = new Component {
      preferredSize = (760 /* 480 */, 120)

      font        = GulimFont(64) // OpenSansFont(64)
      background  = Color.white // Color.black
      foreground  = Color.black // Color.white

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
//        val ry    = -56.0 // r.getMinY // -36.0 // math.min(r.getMinY, r.getMaxY)
        val ry    = mFontSize.getNumber.intValue() * -7.0/8
        g.translate(-rx + 8, -ry + 8)

        val shpMorph  = new ShapeInterpolator
        val f         = fraction.linLin(0, 1, -PiH, PiH).sin.linLin(-1, 1, 0, 1)
        var aIdx      = 0
        var bIdx      = 0
        var tIdx      = 0

        val revA    = ggRevA.selected
        val revB    = ggRevB.selected
        val iShiftA = mShiftA.getNumber.intValue()
        val iShiftB = mShiftB.getNumber.intValue()
        val evenOdd = ggEvenOdd.selected
        val diff    = ggDiff   .selected
        val lines   = mLines.getNumber.intValue()

        while (tIdx < numT) {
          import EditTranscript._
          val edit = trans.charAt(tIdx)
          if (edit == Copy) {
            val shpA  = vecA.getGlyphOutline(aIdx)
            val posA  = vecA.getGlyphPosition(aIdx).getX
            val posB  = vecB.getGlyphPosition(bIdx).getX
            val shp   = if (f == 0) shpA else {
              val dx = f.linLin(0, 1, 0, posB - posA)
              val at = AffineTransform.getTranslateInstance(dx, 0)
              at.createTransformedShape(shpA)
            }
            g.fill(shp)

            // Note: treat S(' ', _) as I and treat S(_, ' ') as D
          } else if (edit == Substitute && txtA.charAt(aIdx) != ' ' && txtB.charAt(bIdx) != ' ') {
            val shpA0 = vecA.getGlyphOutline(aIdx)
            val shpB0 = vecB.getGlyphOutline(bIdx)

            val shpA1   = if (!revA)        shpA0 else Vertex.reverseShape(shpA0)
            val shpB1   = if (!revB)        shpB0 else Vertex.reverseShape(shpB0)
            val shpA3   = if (lines == 0)   shpA1 else Vertex.lines       (shpA1, lines)
            val shpB3   = if (lines == 0)   shpB1 else Vertex.lines       (shpB1, lines)
            val shpA4   = if (!diff)        shpA3 else Vertex.difference  (shpA3)
            val shpB4   = if (!diff)        shpB3 else Vertex.difference  (shpB3)
            val shpA    = if (iShiftA == 0) shpA4 else Vertex.shiftShape("A", shpA4, iShiftA)
            val shpB    = if (iShiftB == 0) shpB4 else Vertex.shiftShape("B", shpB4, iShiftB)

            val shp0    = shpMorph.evaluate(shpA, shpB, f, true)
            val shp     = if (!evenOdd) shp0 else Vertex.changeShapeRule(shp0, evenOdd = true)
            // val shp     = if (!diff)    shp1 else Vertex.difference(shp1)

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

    val pText = new BorderPanel {
      add(new BoxPanel(Orientation.Vertical) {
        contents += ggTextA
        contents += ggTextB
      }, BorderPanel.Position.Center)
      add(ggCopyText, BorderPanel.Position.East)
    }

    val pBot = new BoxPanel(Orientation.Vertical) {
      contents += pText
      contents += new FlowPanel(ggSlider, lbSlider)
      contents += new FlowPanel(ggShiftA, ggRevA)
      contents += new FlowPanel(ggShiftB, ggRevB)
      contents += new FlowPanel(ggResetShift, ggPostShift, new Label("Lines:"), ggLines,
        ggDiff, new Label("Font:"), ggFont, ggFontSize)
      contents += new FlowPanel(ggInvert, ggTimer, ggExportPDF, ggExportPNG)
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