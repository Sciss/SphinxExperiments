/*
 *  VertexImpl.scala
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

import java.awt.Shape

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.InTxn

final class VertexImpl(startTime: Int, phasePeriod: Int, seq: Vec[Option[Shape]]) extends Vertex {
  def tick(time: Int)(implicit tx: InTxn): Unit = {
    val step = ((time - startTime) / phasePeriod) % seq.size
  }

  def move(dx: Double, dy: Double)(implicit tx: InTxn): Unit = ???

  def weight(implicit tx: InTxn): Double = ???

  def bounds(implicit tx: InTxn): DoubleRectangle2D = ???

  def shape(implicit tx: InTxn): Shape = ???
}