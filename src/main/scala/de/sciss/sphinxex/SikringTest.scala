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
import java.awt.{Color, RenderingHints}
import javax.imageio.ImageIO
import javax.swing.{SpinnerNumberModel, Timer}

import de.sciss.file._
import de.sciss.sphinxex.sikring.Renderer
import de.sciss.swingplus.Spinner

import scala.concurrent.stm.atomic
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{BorderPanel, Button, CheckBox, Component, Dimension, FlowPanel, Frame, Graphics2D, Label, MainFrame, SimpleSwingApplication, Swing}

object SikringTest extends SimpleSwingApplication {
  lazy val top: Frame = {
    val phrases = Vec(
//      Vec(
//        // 01234567890123
//        "DIE JUWELEN ES",
//        "DIE VIREN ES",
//        "JUWELEN ES",
//        "THEORIEN ES"
//      ),
//      Vec(
//        "ÜBERLEGT OB ICH ZU GEBEN",
//        "ÜBERLEGT OB ER ZU GEBEN",
//        "GEWÜRDIGT ZU GEBEN",
//        "ÜBERLEGT UND ZU GEBEN"
//      ),
    Vec(
      "EMPÖRTE KANN SOWOHL AUS WAR NICHT MEHR DA",
      "EMPÖRTE KANN SOFORT NUTZBAR NICHT MEHR DA",
      "EMPÖRTE KANN SOWOHL PLUS WAR NICHT MEHR DA"
      // "EMPÖRTE KANN SOFORT NUTZBAR MÜNCHNER DA"
    ),
    Vec(
      "BORN"
    ),
    Vec(
      "DER RICHTER SICH EIN AMT ESEL",
      "DADURCH DASS SIE EIN AMT ESEL",
      "DADURCH PLÖTZLICH EIN AMT ESEL",
      "DADURCH DASS SICH EIN AMT ESEL"
    ),
    Vec(
      "BRAUCHT"
    ),
    Vec(
      "EINE FURCHT"
    ),
    Vec(
      "SIE MAL",
      "GEH MAL",
      "WENN MAL",
      "MAL"
    ),
    Vec(
        "WIEDER NEUE",
        "WIE DER NEUE",
        "WEDER NEUE",
        "B DER NEUE"
    ),
    Vec(
        "ENORM ALTER ZU",
        "WENN EIN ALTER ZU",
        "IM ALTER ZU",
        "EIN ALTER ZU"
    ),
    Vec(
        "FOLGE VON HÄUSERN",
        "VOR VON HÄUSERN",
        "FOLGEN VON HÄUSERN",
        "VORWURF VON HÄUSERN"
    ),
    Vec(
        "RUFEN"
    ),
      Vec(
        "GEH SONST EBEN DIESES",
        "LIEß UNS EBEN DIESES",
        "WIE SONST EBEN DIESES",
        "DIE SONST EBEN DIESES "
      ),
      Vec(
        "SCHLICHT WILDEN AUF DER RECHTEN",
        "SCHLICHT BILDEN AUF DER RECHTEN",
        "SCHLICH BILDEN AUF DER RECHTEN",
        "SCHICHT BILDEN AUF DER RECHTEN"
      ),
      Vec(
        "ABER EIN",
        "WER EIN",
        "EIN",
        "ÜBER EIN"
      ),
      Vec(
        "AUCH EIN",
        "OB EIN",
        "VOR EIN",
        "FRAU EIN"
//      ),
//      Vec(
//        "BEI MÜNCHEN IST HAT",
//        "WIE BEI MÜNCHEN IST HAT",
//        "ÜBER MÖGLICHE WÄCHST HAT",
//        "ÜBER MÜNCHEN IST HAT"
      )
    )

    val graph = Renderer(phrases)
    atomic { implicit tx => graph.zoom = 0.9 }

    def prepareGraphics(g: Graphics2D, w: Int, h: Int): Unit = {
      g.setColor(Color.black)
      g.fillRect(0, 0, w, h)
      g.setColor(Color.white)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
    }

    val comp  = new Component {
      background    = Color.black
      foreground    = Color.white
      preferredSize = new Dimension(1280 /* 1440 */, 84 * phrases.size)
      opaque        = true

      override protected def paintComponent(g: Graphics2D): Unit = {
        prepareGraphics(g, peer.getWidth, peer.getHeight)
        atomic { implicit tx =>
          graph.render(g)
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

    val ggMovie = Button("Export Movie...") {
      val dir = file("image_out") / "video"
      def mkFile(frame: Int) = dir / s"frame-$frame.png"
      if (mkFile(1).exists()) {
        println("Video already exists. Not overwriting!")
      } else {
        val dim = comp.preferredSize
        val img = new BufferedImage(dim.width, dim.height, BufferedImage.TYPE_BYTE_GRAY)
        val g   = img.createGraphics()
        for (fr <- 1 to (25 * 60)) {
          println(s"frame $fr")
          prepareGraphics(g, dim.width, dim.height)
          atomic { implicit tx =>
            graph.tick()
            graph.render(g)
          }
          ImageIO.write(img, "png", mkFile(fr))
        }
      }
    }

    val pBut  = new FlowPanel(ggTick, ggAdvance, new Label("fps:"), ggFPS, ggAnim, ggMovie)

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
