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

import scala.collection.JavaConverters

object Test6 extends App {
  val lucreDir  = file("lucre_out") / "URF-unvorhergesehen.lucre"
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
    println(s"Recovered ${list.size} lattices.")
    val lattices = list.valuesIterator // .toVector
    var minScore = Double.PositiveInfinity
    var maxScore = Double.NegativeInfinity
    var sumScore = 0.0
    var sumSize  = 0

    lattices.foreach { l =>
      import JavaConverters._
      val xs = l.getWordResultPath.asScala.map(_.getWord.getSpelling).mkString(" ")
      println("____")
      println(xs)
      BestPaths(l, n = 10).foreach { p =>
        println(p)
        if (p.score < minScore) minScore = p.score
        if (p.score > maxScore) maxScore = p.score
        sumScore += p.score
        sumSize  += 1
      }
    }

    // Score ranges from -611654726.9 to -55913834.0. Mean is -174670190.4
    // (thus spanning a factor of 10)
    println(f"Score ranges from $minScore%1.1f to $maxScore%1.1f. Mean is ${sumScore / sumSize}%1.1f")

    sys.exit()
  }
}