/*
 *  Test2.scala
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

import java.awt.{Font, Color}
import java.io.FileInputStream

import de.sciss.audiowidgets.AxisFormat
import de.sciss.file._
import de.sciss.pdflitz
import de.sciss.processor.Processor
import de.sciss.synth.io.AudioFile
import edu.cmu.sphinx
import edu.cmu.sphinx.api.StreamSpeechRecognizer

import scala.annotation.tailrec
import scala.collection.JavaConverters
import scala.concurrent.blocking
import scala.swing.{Dimension, Graphics2D}

// -DlogLevel=WARNING
object Test2 extends App {
  val cfg       = new sphinx.api.Configuration
  sys.props("logLevel") = "WARNING"

  val voxDir    = userHome / "Downloads" / "voxforge-de-r20141117"
  val amDir     = voxDir / "model_parameters" / "voxforge.cd_cont_3000"
  val dictFile  = voxDir / "etc" / "voxforge.dic"
  val lmFile    = voxDir / "etc" / "voxforge.lm.DMP"
  val lmFileL   = lmFile.replaceExt("dmp")  // stupid sphinx has case-sensitive check
  val audioFile = userHome / "Downloads" / "unvorhergesehen-mRsmpCut.wav"
  // val audioFile = userHome / "Downloads" / "URF-unvorhergesehen-16k.wav"
  val imageFile = file("image_out") / "test.pdf"

  if (imageFile.exists()) {
    println(s"Image file '$imageFile' already exists. Not overwriting.")
    sys.exit()
  }

  val imgWidth    = 1080
  val imgHeight   = 1920
  val imgInset    = 32
  val wi          = imgWidth  - (imgInset + imgInset)
  val hi          = imgHeight - (imgInset + imgInset)

  require(amDir    .isDirectory)
  require(dictFile .isFile     )
  require(lmFile   .isFile     )
  require(audioFile.isFile     )

  val specIn    = AudioFile.readSpec(audioFile)
  require(specIn.numChannels == 1)
  require(specIn.sampleRate  == 16000.0)
  val duration  = specIn.numFrames / specIn.sampleRate

  val lineWidth   = wi
  val lineHeight  = 24
  val numLines    = hi / lineHeight
  val timeScale   = wi.toDouble * numLines / duration

  if (!lmFileL.exists()) {
    import sys.process._
    // symbolic link with extension to lower case so Sphinx eats it
    val res = Seq("ln", "-sr", lmFile.path, lmFileL.path).!
    require(res == 0, res.toString)
  }

  cfg.setAcousticModelPath(amDir   .path)
  cfg.setDictionaryPath   (dictFile.path)
  cfg.setLanguageModelPath(lmFileL .path)

  val rec   = new StreamSpeechRecognizer(cfg)
  val is    = new FileInputStream(audioFile)

  rec.startRecognition(is)

  val SampleRate  = 16000.0 // sample rate must be 16K, or otherwise funny things happen
  val BlockSize   = 16      // XXX TODO -- is this fixed?
  val timeFmt     = AxisFormat.Time()

  val proc = Processor[Unit]("Generate") { self =>
    val view = pdflitz.Generate.QuickDraw(new Dimension(imgWidth, imgHeight))(process(self, _))
    blocking {
      pdflitz.Generate(file = imageFile, view = view)
    }
  }
  println("_" * 33)
  proc.monitor()
  exitWithProcessor(proc)

  private lazy val _initFont: Font = {
    val url = getClass.getResource("/OpenSans-CondLight.ttf")
    require(url != null)
//    if (url == null) {
//      new Font(Font.SANS_SERIF, Font.PLAIN, 1)
//    } else {
      val is = url.openStream()
      val res = Font.createFont(Font.TRUETYPE_FONT, is)
      is.close()
      res
//    }
    //      // "SF Movie Poster Condensed"
    //      new Font( "BellySansCondensed", Font.PLAIN, 12 )
  }

  private var _condensedFont: Font = _

  def condensedFont: Font = {
    if (_condensedFont == null) _condensedFont = _initFont
    _condensedFont
  }

  def process(self: Processor.Body, g: Graphics2D): Unit = {
    g.setColor(Color.black)
    val font = condensedFont.deriveFont(16) // new Font(Font.SANS_SERIF, Font.PLAIN, 16)
    g.setFont(font)
    val fm   = g.getFontMetrics

    @tailrec def loop(): Unit = Option(rec.getResult) match {
      case Some(speechRes) =>
        val res   = speechRes.getResult
        // val hypo  = res.getBestResultNoFiller
        // res.getCollectTime
        // res.getStartFrame
        import JavaConverters._
        val list  = res.getTimedBestResult(true).asScala
        list.foreach { wordRes =>
          val word    = wordRes.getWord
          val frame   = wordRes.getTimeFrame
          val start   = frame.getStart * BlockSize / SampleRate
          val end     = frame.getEnd   * BlockSize / SampleRate
          // val startF  = timeFmt.format(start)
          // val endF    = timeFmt.format(end  )
          // s"{${word.word}, %.3f, [%s]}", word, LogMath.getLogMath().logToLinear((float)getConfidence()), timeFrame);
          // s"{$word, [$startF - $endF]}"
          if (!word.isSentenceEndWord) {
            val x0    = (start * timeScale).toInt
            val x     = x0 % lineWidth
            val line  = x0 / lineWidth
            val y     = line * lineHeight
            g.drawString(word.getSpelling, x + imgInset, y + imgInset + fm.getAscent)
          }

          self.progress = end / duration
          self.checkAborted()
        }
//        println(listS.mkString(", "))
//        val best = speechRes.getNbest(4).asScala.mkString(" | ")
//        println(best)
  //      val l = speechRes.getLattice
  //      val words2 = l.getWordResultPath.asScala.map { wordRes =>
  //        wordRes.getWord
  //      }
  //      println(words2.mkString(" "))
        loop()
      case None =>
        rec.stopRecognition()
        // println("Done.")
    }
    loop()
  }
}