/*
 *  Test2.scala
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

object German {
  val voxDir    = userHome / "Downloads" / "voxforge-de-r20141117"
  val amDir     = voxDir / "model_parameters" / "voxforge.cd_cont_3000"
  val dictFile  = voxDir / "etc" / "voxforge.dic"
  val lmFile    = voxDir / "etc" / "voxforge.lm.DMP"
  val lmFileL   = lmFile.replaceExt("dmp")  // stupid sphinx has case-sensitive check
}