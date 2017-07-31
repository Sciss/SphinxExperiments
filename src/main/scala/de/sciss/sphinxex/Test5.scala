/*
 *  Test5.scala
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

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB

import scala.util.{Failure, Success}

object Test5 extends App {
  sys.props("logLevel") = "WARNING"

  // val audioFile = userHome / "Downloads" / "unvorhergesehen-mRsmpCut.wav"
  val audioFile = userHome / "Downloads" / "URF-unvorhergesehen-16k.wav"  // the lot
  val lucreDir  = file("lucre_out") / "URF-unvorhergesehen.lucre"
  require(!lucreDir.exists())

  type S        = Durable
  val store     = BerkeleyDB.factory(lucreDir)
  val system    = Durable(store)
  implicit val cursor: stm.Cursor[S] = system
  val factory   = ParseToLucre[S]

  import ParseToLucre.ProductSerializer
  val listH = system.root { implicit tx =>
    ParseToLucre.mkList[S]()
  }

  val config    = ParseToLucre.Config(audioInput = audioFile, listH = listH, verbose = false)
  val proc      = factory(config)
  proc.start()

  println("_" * 33)
  proc.monitor()
  /* val sync = */ mkBlockTread()

  proc.onComplete {
    case Success(()) =>
      sys.exit()
    case Failure(ex) =>
      ex.printStackTrace()
      sys.exit(1)
  }
}