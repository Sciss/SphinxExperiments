/*
 *  HTorqueImpl.scala
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
package impl

import de.sciss.numbers

import scala.concurrent.stm.InTxn

object HTorqueImpl {
  private final val NoForce = (DoublePoint2D.Zero, DoublePoint2D.Zero)
}
final class HTorqueImpl(startTime: Int, phasePeriod: Int, seq: Vec[Double])
  extends Force {

  import HTorqueImpl.NoForce

  def eval(time: Int, edge: Edge)(implicit tx: InTxn): (DoublePoint2D, DoublePoint2D) = {
    import edge.{sink, source}
//    val rA      = source.bounds
    val pA      = source.position
//    val rB      = sink  .bounds
    val pB      = sink  .position

    val dt      = time - startTime
    val step    = (dt / phasePeriod) % seq.size
    val phase0  = (dt % phasePeriod).toDouble / phasePeriod
    import numbers.Implicits._
    val PiH     = math.Pi/2
    val phase   = phase0.linlin(0, 1, -PiH, PiH).sin.linlin(-1, 1, 0, 1)
    val w0      = seq (step)
    val w1      = seq((step + 1) % seq.size)
    val targetH0 = if (w0 == 0) w1 else if (w1 == 0) w0 else w0 * (1 - phase) + w1 * phase
    if (targetH0 == 0) return NoForce
    // if (w0 == 0) return NoForce
    // val targetH = w0 + 16
    val targetH = targetH0 + 16

    // val currH   = (rB.cx + pB.x) - (rA.cx + pA.x)
    val currH   = pB.x - pA.x
    // F = m * a
    // F = -k * x
    val diffH   = targetH - currH
    val weight  = if (w0 == 0) phase else if (w1 == 0) 1 - phase else 1.0
    val dx      = diffH * weight * 0.2 // 0.1
    val dy      = 0.0   // XXX TODO
    val dxA     = -dx/2
    val dxB     =  dx/2
    val dyA     = -dy/2
    val dyB     =  dy/2

    // println(f"${source.label}>${sink.label}: w0 $w0%1.1f w1 $w1%1.1f phase $phase%1.2f weight $weight%1.2f diff $diffH%1.1f dx $dx%1.2f")
    // println(f"${source.label}>${sink.label}: targetH $targetH%1.1f currH $currH%1.1f dxA $dxA%1.1f dxB $dxB%1.1f")

    (DoublePoint2D(dxA, dyA), DoublePoint2D(dxB, dyB))
  }
}
