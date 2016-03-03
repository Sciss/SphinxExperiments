/*
 *  HTorqueImpl.scala
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
package sikring
package impl

import scala.concurrent.stm.InTxn

object HTorqueImpl {
  private final val NoForce = (DoublePoint2D.Zero, DoublePoint2D.Zero)
}
final class HTorqueImpl(startTime: Int, phasePeriod: Int, seq: Vec[Double])
  extends Force {

  import HTorqueImpl.NoForce

  def eval(time: Int, edge: Edge)(implicit tx: InTxn): (DoublePoint2D, DoublePoint2D) = {
    import edge.{sink, source}
    val rA      = source.bounds
    val pA      = source.position
    val rB      = sink  .bounds
    val pB      = sink  .position

    val dt      = time - startTime
    val step    = (dt / phasePeriod) % seq.size
    val phase   = (dt % phasePeriod).toDouble / phasePeriod
    val w0      = seq (step)
    val w1      = seq((step + 1) % seq.size)
    val targetH = if (w0 == 0) w1 else if (w1 == 0) w0 else w0 * (1 - phase) + w1 * phase
    if (targetH == 0) return NoForce

    val currH   = (rB.cx + pB.x) - (rA.cx + pA.x)
    // F = m * a
    // F = -k * x
    val diffH   = targetH - currH
    val weight  = source.weight * sink.weight
    val dx      = diffH * weight * 0.1
    val dy      = 0.0   // XXX TODO
    val dxA     = -dx/2
    val dxB     =  dx/2
    val dyA     = -dy/2
    val dyB     =  dy/2

    println(f"${hashCode().toHexString}%8s: weight $weight%1.3f diff $diffH%1.1f dx $dx%1.3f")

    (DoublePoint2D(dxA, dyA), DoublePoint2D(dxB, dyB))
  }
}
