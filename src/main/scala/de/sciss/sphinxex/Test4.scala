/*
 *  Test4.scala
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

import de.sciss.file._
import de.sciss.processor.Processor
import edu.cmu.sphinx.result.Nbest

import scala.collection.JavaConverters
import scala.concurrent.{Future, blocking}

object Test4 extends App {
  val audioFile = userHome / "Downloads" / "unvorhergesehen-mRsmpCut.wav"
  val jsonFile  = file("json_out") / audioFile.replaceExt("json").name

  val proc = Processor.fromFuture("Read", Future(blocking(ParseToJson.read(jsonFile))))

  proc.monitor(printResult = false)
  proc.onSuccess {
    case ParseToJson.Read(_, lattices) =>
      println(s"Recovered ${lattices.size} lattices.")
      lattices.foreach { l =>
        import JavaConverters._
        val xs = l.getWordResultPath.asScala.map(_.getWord.getSpelling).mkString(" ")
        println("____")
        println(xs)
        new Nbest(l).getNbest(4).asScala.foreach(println)
      }
      sys.exit()
  }
  proc.onFailure {
    case _ =>
      Thread.sleep(200)
      sys.exit(1)
  }

  mkBlockTread() // waitForProcessor(proc)
}