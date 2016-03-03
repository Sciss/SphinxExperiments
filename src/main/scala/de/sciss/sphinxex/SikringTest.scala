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

import java.awt.{Color, RenderingHints}
import javax.swing.{SpinnerNumberModel, Timer}

import de.sciss.sphinxex.sikring.Renderer
import de.sciss.swingplus.Spinner

import scala.concurrent.stm.atomic
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{BorderPanel, Button, CheckBox, Component, Dimension, FlowPanel, Frame, Graphics2D, Label, MainFrame, SimpleSwingApplication, Swing}

object SikringTest extends SimpleSwingApplication {
  lazy val top: Frame = {
    val phrases = Vec(
      Vec(
        // 01234567890123
        "DIE JUWELEN ES",
        "DIE VIREN ES",
        "JUWELEN ES",
        "THEORIEN ES"
      ),
      Vec(
        "ÜBERLEGT OB ICH ZU GEBEN",
        "ÜBERLEGT OB ER ZU GEBEN",
        "GEWÜRDIGT ZU GEBEN",
        "ÜBERLEGT UND ZU GEBEN"
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
      ),
      Vec(
        "BEI MÜNCHEN IST HAT",
        "WIE BEI MÜNCHEN IST HAT",
        "ÜBER MÖGLICHE WÄCHST HAT",
        "ÜBER MÜNCHEN IST HAT"
      )
    )

    val graph = Renderer(phrases)

    val comp  = new Component {
      background    = Color.black
      foreground    = Color.white
      preferredSize = new Dimension(1024, 84 * phrases.size)
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
