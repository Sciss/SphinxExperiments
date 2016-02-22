/*
 *  Test3.scala
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

object Test3 extends App {
  sys.props("logLevel") = "WARNING"

  val audioFile = userHome / "Downloads" / "unvorhergesehen-mRsmpCut.wav"
  // val audioFile = userHome / "Downloads" / "URF-unvorhergesehen-16k.wav"
  val jsonFile  = file("json_out") / audioFile.replaceExt("json").name
  require(!jsonFile.exists() || jsonFile.length == 0L)
  val config    = ParseToJson.Config(audioInput = audioFile, jsonOutput = jsonFile, verbose = false)
  val proc      = ParseToJson(config)
  proc.start()

  println("_" * 33)
  proc.monitor()
  exitWithProcessor(proc)
}
