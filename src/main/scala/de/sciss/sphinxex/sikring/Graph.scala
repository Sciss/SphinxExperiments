/*
 *  Graph.scala
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

import de.sciss.sphinxex.sikring.impl.GraphImpl

import scala.concurrent.stm.InTxn
import scala.swing.Graphics2D

object Graph {
  def apply(): Graph = new GraphImpl
}
trait Graph {
  def addVertex   (v: Vertex)(implicit tx: InTxn): Unit
  def removeVertex(v: Vertex)(implicit tx: InTxn): Unit

  def addEdge     (e: Edge  )(implicit tx: InTxn): Unit
  def removeEdge  (e: Edge  )(implicit tx: InTxn): Unit

  def tick(advance: Boolean = true)(implicit tx: InTxn): Unit

  def pan                        (implicit tx: InTxn): DoublePoint2D
  def pan_=(value: DoublePoint2D)(implicit tx: InTxn): Unit

  def zoom                       (implicit tx: InTxn): Double
  def zoom_=(value: Double      )(implicit tx: InTxn): Unit
  
  def render(g: Graphics2D)(implicit tx: InTxn): Unit
}
