/*
 *  Test6.scala
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
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import edu.cmu.sphinx.result.Nbest

import scala.collection.JavaConverters

object Test6 extends App {
  val audioFile = userHome / "Downloads" / "unvorhergesehen-mRsmpCut.wav"
  val lucreDir  = file("lucre_out") / audioFile.replaceExt("lucre").name
  require(lucreDir.isDirectory)

  type S        = Durable
  val store     = BerkeleyDB.factory(lucreDir, createIfNecessary = false)
  val system    = Durable(store)

  import ParseToLucre.ProductSerializer
  val listH = system.root { implicit tx =>
    ParseToLucre.mkList[S]()
  }

  system.step { implicit tx =>
    val list = listH()
    val lattices = list.valuesIterator.toVector
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
}