/*
 *  BestPaths.scala
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

import edu.cmu.sphinx.result.{BoundedPriorityQueue, Lattice, Node}

import scala.collection.{JavaConverters, mutable}

object BestPaths {
  final class Path(val nodes: Vec[Node], val score: Double, private[BestPaths] val fwdScore: Double)
    extends Comparable[Path] {

    def compareTo(that: Path): Int = score compareTo that.score

    override def toString: String = nodes.map(_.getWord).mkString("", " ", f"[$score%1.3f, $fwdScore%1.3f]")
  }

  def apply(lattice: Lattice, n: Int): Vec[Path] = {
    val result  = mutable.Set.empty[Path]
    val queue   = new BoundedPriorityQueue[Path](n)

    queue.add(new Path(lattice.getInitialNode +: Vector.empty, 0.0, 0.0))

    while (result.size < n && queue.size() > 0) {
      val path = queue.poll()
      if (path.nodes.last == lattice.getTerminalNode) {
        result.add(path)
      } else {
        import JavaConverters._
        for (e <- path.nodes.last.getLeavingEdges.asScala) {
          val newNode     = e.getToNode
          val newFwdScore = path.fwdScore + e.getAcousticScore + e.getLMScore
          val newScore    = newFwdScore + newNode.getBackwardScore
          val newPath     = new Path(path.nodes :+ newNode, newScore, newFwdScore)
          queue.add(newPath)
        }
      }
    }

    result.toVector.sortBy(-_.score)
  }
}