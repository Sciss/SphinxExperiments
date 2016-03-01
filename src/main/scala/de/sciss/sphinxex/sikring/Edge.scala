/*
 *  Edge.scala
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

package de.sciss.sphinxex.sikring

object Edge {
  def apply(source: Vertex, sink: Vertex, force: Force): Edge =
    new Impl(source = source, sink = sink, force = force)

  private[this] final class Impl(val source: Vertex, val sink: Vertex, force: Force)
    extends Edge {

    override def toString = s"Edge($source, $sink, $force)"

    def evalForce: (DoublePoint2D, DoublePoint2D) = force.eval(this)
  }
}
trait Edge {
  def source: Vertex
  def sink  : Vertex

  def evalForce: (DoublePoint2D, DoublePoint2D)
}
