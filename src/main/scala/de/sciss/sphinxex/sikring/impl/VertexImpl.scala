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
import java.awt.geom.{Path2D, PathIterator, AffineTransform}

import de.sciss.{kollflitz, numbers}
import de.sciss.shapeint.ShapeInterpolator
import play.api.libs.json.{Json => PlayJson, JsNumber, JsArray, JsObject}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Ref}

object VertexImpl {
  private[this] sealed trait PathCmd {
    def addTo(p: Path2D): Unit
  }
  private[this] final case class PathMove(x: Double, y: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.moveTo(x, y)
  }
  private[this] final case class PathLine(x: Double, y: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.lineTo(x, y)
  }
  private[this] final case class PathQuad(x1: Double, y1: Double, x2: Double, y2: Double) extends PathCmd {
    def addTo(p: Path2D): Unit = p.quadTo(x1, y1, x2, y2)
  }
  private[this] final case class PathCube(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double)
    extends PathCmd {

    def addTo(p: Path2D): Unit = p.curveTo(x1, y1, x2, y2, x3, y3)
  }
  private[this] case object PathClose extends PathCmd {
    def addTo(p: Path2D): Unit = p.closePath()
  }

  private lazy val morphPairs: Map[(Char, Char), (Int, Int)] = {
    val url   = getClass.getResource("/open-sans-morph-nocomment.json")
    val is    = url.openStream()
    val len   = is.available()
    val arr   = new Array[Byte](len)
    is.read(arr)
    is.close()
    val JsObject(fields) = PlayJson.parse(arr)
    val map: Map[(Char, Char), (Int, Int)] = fields.map {
      case (ab, JsArray(Seq(JsNumber(n1), JsNumber(n2), _ @ _*))) =>
        (ab(0), ab(1)) -> ((n1.intValue(), n2.intValue()))
    } (breakOut)
    map.withDefaultValue((0, 0))
  }

  def shiftShape(name: String, shp: Shape, shift: Int): Shape = {
    val it  = shp.getPathIterator(null)
    val vb  = Vector.newBuilder[PathCmd]
    val c   = new Array[Double](6)
    // first create a collection of commands
    while (!it.isDone) {
      import PathIterator._
      val cmd = it.currentSegment(c) match {
        case SEG_MOVETO  => PathMove(c(0), c(1))
        case SEG_LINETO  => PathLine(c(0), c(1))
        case SEG_QUADTO  => PathQuad(c(0), c(1), c(2), c(3))
        case SEG_CUBICTO => PathCube(c(0), c(1), c(2), c(3), c(4), c(5))
        case SEG_CLOSE   => PathClose
      }
      vb += cmd
      it.next()
    }
    val v = vb.result()

    import kollflitz.Ops._
    val itGroup = v.groupWith {
      case (PathClose, PathMove(_, _)) => false
      case _ => true
    }
    val head = itGroup.next()

    val segm = head.groupWith {
      case (_, PathLine(_, _)) => false
      case _ => true
    } .toIndexedSeq

    // println("----SEGM----")
    // segm.foreach(println)

    if (shift >= segm.size) println(s"WARNING: $name segm.size = ${segm.size}")

    val iShift1 = shift % segm.size
    val res     = if (iShift1 == 0) shp else {
      val (pre, suf)  = segm.splitAt(iShift1)
      val (PathMove(x0, y0) +: preTail) = pre.flatten
      val (PathLine(x1, y1) +: sufInner :+ PathClose) = suf.flatten
      val p = new Path2D.Float()
      p.moveTo(x1, y1)
      sufInner.foreach(_.addTo(p))
      p.lineTo(x0, y0)
      preTail.foreach(_.addTo(p))
      p.closePath()
      itGroup.foreach { commands =>
        commands.foreach(_.addTo(p))
      }
      p
    }

    res
  }
}
final class VertexImpl(val label: String, startTime: Int, phasePeriod: Int, seq: Vec[(Char, Shape)])
  extends Vertex {

  import Vertex.EmptyShape
  import VertexImpl.morphPairs

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
    // val phase   = (dt % phasePeriod).toDouble / phasePeriod
    val phase0  = (dt % phasePeriod).toDouble / phasePeriod
    val PiH     = math.Pi/2
    import numbers.Implicits._
    val phase   = phase0.linlin(0, 1, -PiH, PiH).sin.linlin(-1, 1, 0, 1)
    val w0      = if (seq (step)._2                 .isEmpty) 0.0 else 1.0
    val w1      = if (seq((step + 1) % seq.size)._2 .isEmpty) 0.0 else 1.0
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
    val (charA, shpAOpt) = seq (step)
    val (charB, shpBOpt) = seq((step + 1) % seq.size)
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
          val phase = phaseRef().toFloat
          if (phase == 0f) shpA else if (phase == 1f) shpB else {
            val (shiftA, shiftB) = morphPairs((charA, charB))
            val shpAS = if (shiftA == 0) shpA else Vertex.shiftShape("A", shpA, shiftA)
            val shpBS = if (shiftB == 0) shpB else Vertex.shiftShape("B", shpB, shiftB)
            ShapeInterpolator.apply(shpAS, shpBS, phase, true)
          }
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