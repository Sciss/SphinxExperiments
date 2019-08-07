/*
 *  German.scala
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

import java.awt.Color
import java.io.FileInputStream

import de.sciss.audiowidgets.AxisFormat
import de.sciss.file._
import de.sciss.pdflitz
import de.sciss.processor.Processor
import de.sciss.synth.io.AudioFile
import edu.cmu.sphinx
import edu.cmu.sphinx.api.StreamSpeechRecognizer
import edu.cmu.sphinx.decoder.search

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
    if (imageFile.length() > 0L) {
      println(s"Image file '$imageFile' already exists. Not overwriting.")
      sys.exit()
    } else {
      imageFile.delete()
    }
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

  def process(self: Processor.Body, g: Graphics2D): Unit = {
    g.setColor(Color.black)
    val font = OpenSansFont(16f)
    g.setFont(font)
    val fm   = g.getFontMetrics

    var allTokens = Set.empty[search.Token]

    @tailrec def loop(): Unit = Option(rec.getResult) match {
      case Some(speechRes) =>
        import JavaConverters._
        val res   = speechRes.getResult

        // typically 3 to 60
        // println(s"NUM NODES = ${speechRes.getLattice.getNodes.size()}")

        // edu.cmu.sphinx.frontend.FloatData
        // Option(res.getDataFrames).foreach(f => f.asScala.headOption.foreach(d => println(s"DATA: ${d.getClass.getName} - $d")))

        // val hypo  = res.getBestResultNoFiller
        // res.getCollectTime
        // res.getStartFrame

        val tokens  = (res.getActiveTokens.asScala.toList ++ res.getResultTokens.asScala.toList).toSet
//        val states  = tokens.map(_.getSearchState)
        // val nullSet = tokens.map(_.getPredecessor != null)
        // println(nullSet.mkString("Null set: ", ", ", ""))
        // val hasOutside = tokens.exists(t => !tokens.contains(t.getPredecessor))
        // println(s"hasOutside = $hasOutside")
        // val acousSet = tokens.map(_.getAcousticScore)
        // println(acousSet.mkString("Acous set: ", ", ", ""))

        val prevTokens  = allTokens
        allTokens       = tokens
        @tailrec
        def loopTokens(t: search.Token): Unit =
          Option(t.getPredecessor) match {
            case Some(pred) =>
              if (allTokens.contains(pred)) {
                allTokens += pred
                loopTokens(pred)
              }
            case None =>
          }

        tokens.foreach(loopTokens)
        val oldTokens = prevTokens intersect allTokens
        println(s"Num tokens = ${allTokens.size}; Old tokens = ${oldTokens.size}")
        val stateNames = allTokens.map(_.getSearchState.getClass.getSimpleName)
        println(stateNames.mkString("States: ", ", ", ""))

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