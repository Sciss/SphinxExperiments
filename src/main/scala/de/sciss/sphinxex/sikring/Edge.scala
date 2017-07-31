/*
 *  Edge.scala
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

package de.sciss.sphinxex.sikring

import scala.concurrent.stm.InTxn

object Edge {
  def apply(source: Vertex, sink: Vertex, force: Force): Edge =
    new Impl(source = source, sink = sink, force = force)

  private[this] final class Impl(val source: Vertex, val sink: Vertex, force: Force)
    extends Edge {

    override def toString = s"Edge($source, $sink, $force)"

    def evalForce(time: Int)(implicit tx: InTxn): (DoublePoint2D, DoublePoint2D) =
      force.eval(time = time, edge = this)
  }
}
trait Edge {
  def source: Vertex
  def sink  : Vertex

  def evalForce(time: Int)(implicit tx: InTxn): (DoublePoint2D, DoublePoint2D)
}
