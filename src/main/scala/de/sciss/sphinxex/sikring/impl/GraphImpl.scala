/*
 *  GraphImpl.scala
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

import scala.concurrent.stm.{Ref, TMap, TSet, InTxn}
import scala.swing.Graphics2D

final class GraphImpl extends Graph {
  private[this] val vertices  = TSet.empty[Vertex]
  private[this] val edges     = TSet.empty[Edge  ]
  private[this] val edgeMap   = TMap.empty[Vertex, Set[Edge]]
  private[this] val zoomRef   = Ref(1.0)
  private[this] val panRef    = Ref(DoublePoint2D.Zero)
  private[this] val timeRef   = Ref(0)

  def addVertex(v: Vertex)(implicit tx: InTxn): Unit = {
    val isNew = vertices.add(v)
    v.tick(timeRef())
    require(isNew)
  }

  def removeVertex(v: Vertex)(implicit tx: InTxn): Unit =
    if (vertices.remove(v)) {
      edgeMap.remove(v).getOrElse(Set.empty).foreach(removeEdge)
    }

  def addEdge(e: Edge)(implicit tx: InTxn): Unit = {
    require(e.source != e.sink         , s"Unsupported edge cycles: $e")
    require(vertices.contains(e.source), s"Source vertex not in graph: $e")
    require(vertices.contains(e.sink  ), s"Sink vertex not in graph: $e")

    val isNew = edges.add(e)
    require(isNew)
    val sourceMap = edgeMap.getOrElse(e.source, Set.empty) + e
    edgeMap.put(e.source, sourceMap)
    val sinkMap   = edgeMap.getOrElse(e.sink  , Set.empty) + e
    edgeMap.put(e.sink, sinkMap)
  }

  def removeEdge(e: Edge)(implicit tx: InTxn): Unit =
    if (edges.remove(e)) {
      val sourceMap = edgeMap.getOrElse(e.source, Set.empty) - e
      if (sourceMap.isEmpty) edgeMap.remove(e.source) else edgeMap.put(e.source, sourceMap)
      val sinkMap   = edgeMap.getOrElse(e.sink  , Set.empty) - e
      if (sinkMap  .isEmpty) edgeMap.remove(e.sink  ) else edgeMap.put(e.sink  , sinkMap  )
    }

  def tick()(implicit tx: InTxn): Unit = {
    val time = timeRef.transformAndGet(_ + 1)
    vertices.foreach(_.tick(time))
    // edges...
  }

  def zoom                        (implicit tx: InTxn): Double        = zoomRef()
  def zoom_=(value: Double       )(implicit tx: InTxn): Unit          = zoomRef() = value

  def pan                         (implicit tx: InTxn): DoublePoint2D = panRef()
  def pan_= (value: DoublePoint2D)(implicit tx: InTxn): Unit          = panRef()  = value

  def render(g: Graphics2D)(implicit tx: InTxn): Unit = {
    val atOrig  = g.getTransform
    val pan     = panRef()
    g.translate(pan.x, pan.y)
    val zoom    = zoomRef()
    g.scale(zoom, zoom)
    vertices.foreach { v =>
      val shp   = v.shape
      val pos   = v.position
      val atNow = g.getTransform
      g.translate(pos.x, pos.y)
      g.fill(shp)
      g.setTransform(atNow)
    }
    g.setTransform(atOrig)
  }
}