/*
 *  Test1.scala
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

import java.io.FileInputStream

import de.sciss.audiowidgets.AxisFormat
import de.sciss.file._
import edu.cmu.sphinx.api.{Configuration, StreamSpeechRecognizer}

import scala.annotation.tailrec
import scala.collection.JavaConverters

// -DlogLevel=WARNING
object Test1 extends App {
  val cfg = new Configuration
  sys.props("logLevel") = "WARNING"

  cfg.setAcousticModelPath("resource:/edu/cmu/sphinx/models/en-us/en-us")
  cfg.setDictionaryPath   ("resource:/edu/cmu/sphinx/models/en-us/cmudict-en-us.dict")
  cfg.setLanguageModelPath("resource:/edu/cmu/sphinx/models/en-us/en-us.lm.bin")

  val rec   = new StreamSpeechRecognizer(cfg)
  // val fIn   = userHome / "Downloads" / "Big_Ego_12-burroughsRsmp.wav"
  val fIn   = userHome / "Documents" / "projects" / "Transpositions"/ "audio_work" / "B18h00m41s08mar2016-MonoRsmp.wav"
  val is    = new FileInputStream(fIn)

  rec.startRecognition(is)

  val SampleRate  = 16000.0 // sample rate must be 16K, or otherwise funny things happen
  val BlockSize   = 16      // XXX TODO -- is this fixed?
  val timeFmt     = AxisFormat.Time()

  @tailrec def loop(): Unit = Option(rec.getResult) match {
    case Some(speechRes) =>
      val res   = speechRes.getResult
      // val hypo  = res.getBestResultNoFiller
      // res.getCollectTime
      // res.getStartFrame
      import JavaConverters._
      val list  = res.getTimedBestResult(true).asScala
      val listS = list.map { wordRes =>
        val word    = wordRes.getWord
        val frame   = wordRes.getTimeFrame
        val start   = frame.getStart * BlockSize / SampleRate
        val end     = frame.getEnd   * BlockSize / SampleRate
        val startF  = timeFmt.format(start)
        val endF    = timeFmt.format(end  )
        // s"{${word.word}, %.3f, [%s]}", word, LogMath.getLogMath().logToLinear((float)getConfidence()), timeFrame);
        word // s"{$word, [$startF - $endF]}"
      }
      // println(listS.mkString(", "))
      println(listS.init.mkString(" "))
      loop()
    case None =>
      rec.stopRecognition()
      println("Done.")
  }
  loop()
}