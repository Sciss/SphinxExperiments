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
import java.awt.geom.AffineTransform

import de.sciss.shapeint.ShapeInterpolator

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Ref}

final class VertexImpl(val label: String, startTime: Int, phasePeriod: Int, seq: Vec[Shape])
  extends Vertex {

  import Vertex.EmptyShape

  private[this] val stepRef   = Ref(0)
  private[this] val phaseRef  = Ref(0.0)
  private[this] val weightRef = Ref(0.0)
  private[this] val posRef    = Ref(DoublePoint2D.Zero)

  private[this] val validRef  = Ref(false)
  private[this] val shapeRef  = Ref.make[Shape]             // depends on validRef
  private[this] val boundsRef = Ref.make[DoubleRectangle2D] // depends on validRef

  def tick(time: Int)(implicit tx: InTxn): Unit = {
    val dt      = time - startTime
    val step    = (dt / phasePeriod) % seq.size
    val phase   = (dt % phasePeriod).toDouble / phasePeriod
    val w0      = if (seq (step)                 .isEmpty) 0.0 else 1.0
    val w1      = if (seq((step + 1) % seq.size) .isEmpty) 0.0 else 1.0
    val weight  = w0 * (1 - phase) + w1 * phase
    stepRef()   = step
    phaseRef()  = phase
    weightRef() = weight
    invalidate()
  }

  @inline
  private[this] def invalidate()(implicit tx: InTxn): Unit = validRef() = false

  @inline
  private[this] def validate()(implicit tx: InTxn): Unit = if (!validRef()) {
    val step    = stepRef()
    val shpAOpt = seq (step)
    val shpBOpt = seq((step + 1) % seq.size)
    val shp     = (shpAOpt, shpBOpt) match {
      case (EmptyShape, EmptyShape) =>
        EmptyShape

      case (shpA, EmptyShape) =>
        val r     = shpA.getBounds2D
        val scale = weightRef()
        val at    = AffineTransform.getTranslateInstance(r.getCenterX, r.getCenterY)
        at.scale(scale, scale)
        at.translate(-r.getCenterX, -r.getCenterY)
        at.createTransformedShape(shpA)

      case (EmptyShape, shpB) =>
        val r     = shpB.getBounds2D
        val scale = weightRef()
        val at    = AffineTransform.getTranslateInstance(r.getCenterX, r.getCenterY)
        at.scale(scale, scale)
        at.translate(-r.getCenterX, -r.getCenterY)
        at.createTransformedShape(shpB)

      case (shpA, shpB) =>
        if (shpA == shpB) shpA else {
          ShapeInterpolator.apply(shpA, shpB, phaseRef().toFloat, true)
        }
    }
    shapeRef()  = shp
    val r       = shp.getBounds2D
    boundsRef() = DoubleRectangle2D(r.getX, r.getY, r.getWidth, r.getHeight)
    validRef()  = true
  }

  def position                    (implicit tx: InTxn): DoublePoint2D = posRef()
  def position_=(p: DoublePoint2D)(implicit tx: InTxn): Unit          = posRef() = p

  def weight(implicit tx: InTxn): Double = weightRef()

  def bounds(implicit tx: InTxn): DoubleRectangle2D = {
    validate()
    boundsRef()
  }

  def shape(implicit tx: InTxn): Shape = {
    validate()
    shapeRef()
  }
}