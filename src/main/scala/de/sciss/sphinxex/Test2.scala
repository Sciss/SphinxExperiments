package de.sciss.sphinxex

import java.io.FileInputStream

import de.sciss.audiowidgets.AxisFormat
import de.sciss.file._
import edu.cmu.sphinx.api.{Configuration, StreamSpeechRecognizer}

import scala.annotation.tailrec
import scala.collection.JavaConverters

// -DlogLevel=WARNING
object Test2 extends App {
  val cfg = new Configuration

  val voxDir    = userHome / "Downloads" / "voxforge-de-r20141117"
  val amDir     = voxDir / "model_parameters" / "voxforge.cd_cont_3000"
  val dictFile  = voxDir / "etc" / "voxforge.dic"
  val lmFile    = voxDir / "etc" / "voxforge.lm.DMP"
  val lmFileL   = lmFile.replaceExt("dmp")  // stupid sphinx has case-sensitive check

  require(amDir   .isDirectory)
  require(dictFile.isFile     )
  require(lmFile  .isFile     )

  if (!lmFileL.exists()) {
    import sys.process._
    //
    val res = Seq("ln", "-sr", lmFile.path, lmFileL.path).!
    require(res == 0, res.toString)
  }

  cfg.setAcousticModelPath(amDir   .path)
  cfg.setDictionaryPath   (dictFile.path)
  cfg.setLanguageModelPath(lmFileL .path)

  val rec   = new StreamSpeechRecognizer(cfg)
  val fIn   = userHome / "Downloads" / "unvorhergesehen-mRsmp.wav" // "unvorhergesehen-mRsmpCut.wav"
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
        s"{$word, [$startF - $endF]}"
      }
      println(listS.mkString(", "))
      loop()
    case None =>
      rec.stopRecognition()
      println("Done.")
  }
  loop()
}