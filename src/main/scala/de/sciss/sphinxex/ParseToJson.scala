/*
 *  ParseToJson.scala
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

import java.io.{FileOutputStream, FileInputStream}

import de.sciss.audiowidgets.AxisFormat
import de.sciss.file._
import de.sciss.play.json.Formats.FileFormat
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}
import de.sciss.synth.io.AudioFile
import edu.cmu.sphinx
import edu.cmu.sphinx.api.StreamSpeechRecognizer
import play.api.libs.json.{JsArray, JsObject, JsValue}

import scala.annotation.tailrec
import scala.collection.JavaConverters
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.blocking

object ParseToJson extends ProcessorFactory {
  case class Config(audioInput: File, jsonOutput: File, verbose: Boolean = false)

  type Product  = Unit
  type Repr     = ParseToJson

  protected def prepare(config: Config): Prepared = new Impl(config)
  
  @inline
  private[this] val BlockSize = 16

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    protected def body(): Unit = blocking {
      import config._
      val fos   = new FileOutputStream(jsonOutput)
      try {
        val recConfig = new sphinx.api.Configuration
        import German._
        recConfig.setAcousticModelPath(amDir   .path)
        recConfig.setDictionaryPath   (dictFile.path)
        recConfig.setLanguageModelPath(lmFileL .path)
        val specIn  = AudioFile.readSpec(audioInput)
        require(specIn.numChannels == 1)
        require(specIn.sampleRate  == 16000.0)
        val duration  = specIn.numFrames / specIn.sampleRate

        import specIn.sampleRate
        require(sampleRate == 16000.0)
        val is      = new FileInputStream(audioInput)
        val rec     = new StreamSpeechRecognizer(recConfig)
        val timeFmt = AxisFormat.Time()

        @tailrec def loop(res: Vec[JsValue]): Vec[JsValue] = Option(rec.getResult) match {
          case Some(speechRes) =>
            import JavaConverters._
            val l           = speechRes.getLattice
            val jsonLattice = Json.LatticeFormat.writes(l)
            // val endFrame    = l.getTerminalNode.getEndTime
            val words       = speechRes.getWords.asScala
            val endFrame    = words.lastOption.map(_.getTimeFrame.getEnd).getOrElse(0L)
            // val endFrame    = speechRes.getResult.getEndFrame
            val end         = endFrame * BlockSize / sampleRate
            progress        = end / duration
            if (verbose) {
              val info = words.mkString(s"${timeFmt.format(end)}: ", ", ", "")
              println(info)
            }
            checkAborted()
            loop(res :+ jsonLattice)
          case None =>
            res
        }

        try {
          rec.startRecognition(is)
          val lattices = loop(Vector.empty)
          val o = JsObject(Seq(
            "audio"     -> FileFormat.writes(audioInput),
            "lattices"  -> JsArray(lattices)
          ))
          val json  = play.api.libs.json.Json.prettyPrint(o)
          // val json  = o.toString()
          fos.write(json.getBytes("UTF-8"))

        } finally {
          rec.stopRecognition()
        }
      } finally {
        fos.close()
      }
    }
  }
}
trait ParseToJson extends ProcessorLike[ParseToJson.Product, ParseToJson] {
  def config: ParseToJson.Config
}