/*
 *  Vertex.scala
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

import java.awt.Shape

import de.sciss.sphinxex.sikring.impl.VertexImpl

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.InTxn

object Vertex {
  def apply(startTime: Int, phasePeriod: Int, seq: Vec[Option[Shape]]): Vertex =
    new VertexImpl(startTime = startTime, phasePeriod = phasePeriod, seq = seq)
}
trait Vertex {
  // def startTime: Int

  def tick(time: Int)(implicit tx: InTxn): Unit

  /** Shape _not_ offset by `position`. */
  def shape(implicit tx: InTxn): Shape

  /** Bounds _not_ offset by `position`. */
  def bounds(implicit tx: InTxn): DoubleRectangle2D

  def position                    (implicit tx: InTxn): DoublePoint2D
  def position_=(p: DoublePoint2D)(implicit tx: InTxn): Unit

  def weight(implicit tx: InTxn): Double
}
