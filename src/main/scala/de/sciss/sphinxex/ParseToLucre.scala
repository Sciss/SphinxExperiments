/*
 *  ParseToLucre.scala
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
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{NoSys, Sys}
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import de.sciss.synth.io.AudioFile
import edu.cmu.sphinx
import edu.cmu.sphinx.api.StreamSpeechRecognizer
import edu.cmu.sphinx.result.Lattice

import scala.annotation.tailrec
import scala.collection.JavaConverters
import scala.concurrent.blocking

object ParseToLucre {
  case class Config(audioInput: File, verbose: Boolean = false)

  type Product[S <: Sys[S]] = stm.Source[S#Tx, SkipList.Map[S, Int, Lattice]]

  trait Factory[S <: Sys[S]] extends ProcessorFactory {
    type Product  = ParseToLucre.Product[S]
    type Repr     = ParseToLucre[S]
    type Config   = ParseToLucre.Config
  }

  def apply[S <: Sys[S]](implicit cursor: stm.Cursor[S]): Factory[S] = new Factory[S] {
    protected def prepare(config: Config): Prepared = new Impl[S](config)
  }

  // Why the hell we didn't define this in Lucre?
  private implicit def skipListSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SkipList.Map[S, Int, Lattice]] =
    anySkipListSer.asInstanceOf[SkipListSer[S]]

  private[this] val anySkipListSer = new SkipListSer[NoSys]

  private[this] final class SkipListSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, SkipList.Map[S, Int, Lattice]] {
    def write(v: SkipList.Map[S, Int, Lattice], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): SkipList.Map[S, Int, Lattice] = {
      import Lucre.LatticeSerializer
      SkipList.Map.read[S, Int, Lattice](in, access)
    }
  }

  @inline
  private[this] val BlockSize = 16

  private final class Impl[S <: Sys[S]](val config: Config)(implicit cursor: stm.Cursor[S])
    extends ProcessorImpl[Product[S], ParseToLucre[S]] with ParseToLucre[S] {

    protected def body(): Product[S] = blocking {
      import config._

      val listH = cursor.step { implicit tx =>
        import Lucre.LatticeSerializer
        val list = SkipList.Map.empty[S, Int, Lattice]
        tx.newHandle(list)
      }

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

      @tailrec def loop(idx: Int): Unit = Option(rec.getResult) match {
        case Some(speechRes) =>
          import JavaConverters._
          val l = speechRes.getLattice
          cursor.step { implicit tx =>
            val list = listH()
            list.add(idx -> l)
          }
          val words       = speechRes.getWords.asScala
          val endFrame    = words.lastOption.map(_.getTimeFrame.getEnd).getOrElse(0L)
          val end         = endFrame * BlockSize / sampleRate
          progress        = end / duration
          if (verbose) {
            val info = words.mkString(s"${timeFmt.format(end)}: ", ", ", "")
            println(info)
          }
          checkAborted()
          loop(idx + 1)
        case None =>
      }

      try {
        rec.startRecognition(is)
        loop(0)
        listH
      } finally {
        rec.stopRecognition()
      }
    }
  }
}
trait ParseToLucre[S <: Sys[S]] extends ProcessorLike[ParseToLucre.Product[S], ParseToLucre[S]] {
  def config: ParseToLucre.Config
}