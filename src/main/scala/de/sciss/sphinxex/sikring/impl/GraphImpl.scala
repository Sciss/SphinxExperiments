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

import scala.collection.mutable
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

  def tick(advance: Boolean)(implicit tx: InTxn): Unit = {
    val time = if (advance) timeRef.transformAndGet(_ + 1) else timeRef()
    vertices.foreach(_.tick(time))

    final class Acc(var x: Double = 0.0, var y: Double = 0.0) {
      def += (p: DoublePoint2D): Unit = {
        x += p.x
        y += p.y
      }
      def toPoint: DoublePoint2D = DoublePoint2D(x, y)
    }

    val acc = mutable.Map.empty[Vertex, Acc] // .withDefaultValue(new Acc)
    edges.foreach { e =>
      val (sourceF, sinkF) = e.evalForce(time)
      acc.getOrElseUpdate(e.source, new Acc) += sourceF
      acc.getOrElseUpdate(e.sink  , new Acc) += sinkF
    }
    // println(s"# edges = ${edges.size} - acc.size ${acc.size}")
    acc.foreach { case (v, a) =>
      // println(f"Vertex ${v.label}: ${a.x}%1.3f")
      v.position = v.position + a.toPoint
    }
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