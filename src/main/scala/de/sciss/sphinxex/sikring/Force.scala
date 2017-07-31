/*
 *  Force.scala
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
package sikring

import de.sciss.sphinxex.sikring.impl.HTorqueImpl

import scala.concurrent.stm.InTxn

object Force {
  def HTorque(startTime: Int, phasePeriod: Int, seq: Vec[Double]): Force =
    new HTorqueImpl(startTime = startTime, phasePeriod = phasePeriod, seq = seq)
}
trait Force {
  def eval(time: Int, edge: Edge)(implicit tx: InTxn): (DoublePoint2D, DoublePoint2D)
}