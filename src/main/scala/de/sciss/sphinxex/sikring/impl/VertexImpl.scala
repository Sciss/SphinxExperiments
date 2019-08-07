/*
 *  VertexImpl.scala
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
package impl

import java.awt.Shape
import java.awt.geom.{AffineTransform, Area, Path2D, PathIterator}

import de.sciss.{kollflitz, numbers}
import de.sciss.shapeint.ShapeInterpolator
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json => PlayJson}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Ref}

object VertexImpl {
  sealed trait PathCmd {
    def addTo(p: Path2D): Unit

    def toLines(startX: Double, startY: Double, n: Int): Vector[PathCmd]
  }
  sealed trait PathCmdTo extends PathCmd {
    def endX: Double
    def endY: Double

    def reverse(startX: Double, startY: Double): PathCmdTo
  }
  sealed trait PathCanInterpolate extends PathCmdTo {
    def interpolate(startX: Double, startY: Double, t: Double): PathLine

    override def toLines(startX: Double, startY: Double, n: Int): Vector[PathLine] =
      if (n == 0) Vector(PathLine(endX, endY)) else {
        val fin = PathLine(endX, endY)
        Vector.tabulate(n) { i =>
          if (i == n - 1) fin else {
            import numbers.Implicits._
            val t = (i + 1).linLin(0, n, 0.0, 1.0)
            interpolate(startX, startY, t)
          }
        }
      }
  }

  final case class PathMove(x: Double, y: Double) extends PathCmdTo {
    def addTo(p: Path2D): Unit = p.moveTo(x, y)
    def endX: Double = x
    def endY: Double = y

    def reverse(startX: Double, startY: Double): PathMove = PathMove(startX, startY)

    def toLines(startX: Double, startY: Double, n: Int): Vector[PathCmd] = Vector(this)
  }
  final case class PathLine(x: Double, y: Double) extends PathCanInterpolate {
    def addTo(p: Path2D): Unit = p.lineTo(x, y)
    def endX: Double = x
    def endY: Double = y

    def reverse(startX: Double, startY: Double): PathLine = PathLine(startX, startY)

    def interpolate(startX: Double, startY: Double, t: Double): PathLine = {
      require(t >= 0 && t <= 1)
      import numbers.Implicits._
      val xi = t.linLin(0, 1, startX, x)
      val yi = t.linLin(0, 1, startY, y)
      PathLine(xi, yi)
    }
  }
  final case class PathQuad(x1: Double, y1: Double, x2: Double, y2: Double) extends PathCanInterpolate {
    def addTo(p: Path2D): Unit = p.quadTo(x1, y1, x2, y2)
    def endX: Double = x2
    def endY: Double = y2

    def reverse(startX: Double, startY: Double): PathQuad =
      PathQuad(x1, y1, startX, startY)

    def interpolate(startX: Double, startY: Double, t: Double): PathLine = {
      require(t >= 0 && t <= 1)
      // P(t) = B(2,0)*CP + B(2,1)*P1 + B(2,2)*P2
      val b20 = B(t, c20, 2, 0)
      val b21 = B(t, c21, 2, 1)
      val b22 = B(t, c22, 2, 2)
      val x   = b20 * startX + b21 * x1 + b22 * x2
      val y   = b20 * startY + b21 * y1 + b22 * y2
      PathLine(x, y)
    }
  }

  private def B(t: Double, c: Double, n: Int, m: Int): Double = c * math.pow(t, m) * math.pow(1 - t, n - m)

  private def factorial(n: Int): Int = {
    require (n >= 0)
    var res = 1
    var m = n
    while (m > 0) {
      res *= m
      m -= 1
    }
    res
  }

  private def C(n: Int, m: Int): Double = factorial(n).toDouble / (factorial(m) * factorial(n - m))

  private[this] val c20 = C(2, 0)
  private[this] val c21 = C(2, 1)
  private[this] val c22 = C(2, 2)
  private[this] val c30 = C(3, 0)
  private[this] val c31 = C(3, 1)
  private[this] val c32 = C(3, 2)
  private[this] val c33 = C(3, 3)

  final case class PathCube(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double)
    extends PathCanInterpolate {

    def addTo(p: Path2D): Unit = p.curveTo(x1, y1, x2, y2, x3, y3)
    def endX: Double = x3
    def endY: Double = y3

    def reverse(startX: Double, startY: Double): PathCube =
      PathCube(x2, y2, x1, y1, startX, startY)

    def interpolate(startX: Double, startY: Double, t: Double): PathLine = {
      require (t >= 0 && t <= 1)
      // P(t) = B(3,0)*CP + B(3,1)*P1 + B(3,2)*P2 + B(3,3)*P3 ; 0 <=t<=1
      val b30 = B(t, c30, 3, 0)
      val b31 = B(t, c31, 3, 1)
      val b32 = B(t, c32, 3, 2)
      val b33 = B(t, c33, 3, 3)
      val x   = b30 * startX + b31 * x1 + b32 * x2 + b33 * x3
      val y   = b30 * startY + b31 * y1 + b32 * y2 + b33 * y3
      PathLine(x, y)
    }
  }
  case object PathClose extends PathCmd {
    def addTo(p: Path2D): Unit = p.closePath()

    def toLines(startX: Double, startY: Double, n: Int): Vector[PathCmd] = Vector(this)
  }

  private lazy val morphPairs: Map[(Char, Char), (Int, Int)] = {
    val url   = getClass.getResource("/open-sans-morph-nocomment.json")
    val is    = url.openStream()
    val len   = is.available()
    val arr   = new Array[Byte](len)
    is.read(arr)
    is.close()
    val JsObject(fields) = PlayJson.parse(arr)
    var map: Map[(Char, Char), (Int, Int)] = fields.map {
      case (ab, JsArray(Seq(JsNumber(n1), JsNumber(n2), _ @ _*))) =>
        (ab(0), ab(1)) -> ((n1.intValue(), n2.intValue()))
    } (breakOut)

    map.iterator.foreach { case ((a, b), (x, y)) =>
      val revKey = (b, a)
      val revVal = (y, x)
      map += revKey -> revVal
    }

    // println(map)

    map.withDefaultValue((0, 0))
  }

  private def mkCmd(shp: Shape): Vector[PathCmd] = {
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
    val v: Vector[PathCmd] = vb.result()
    v
  }

  private def mkGroups(v: Vector[PathCmd]): Iterator[Vector[PathCmd]] = {
    import kollflitz.Ops._
    v.groupWith {
      case (PathClose, PathMove(_, _)) => false
      case _ => true
    }
  }

  def reverseShape(shp: Shape): Shape = {
    val v       = mkCmd(shp)
    val itGroup = mkGroups(v).toVector
    val cmdOut = itGroup.flatMap { group =>
      val (move @ PathMove(x0, y0)) +: init :+ PathClose = group
      val (x1, y1, rev) = ((x0, y0, Vector.empty[PathCmd]) /: init) {
        case ((startX, startY, res), cmd: PathCmdTo) =>
          (cmd.endX, cmd.endY, cmd.reverse(startX, startY) +: res)
        case _ => throw new IllegalStateException
      }

      val seq0 = rev :+ PathClose
      val seq1 = if (x1 == x0 && y1 == y0) seq0 else PathLine(x1, y1) +: seq0
      move +: seq1
    }

    val p = new Path2D.Float()
    cmdOut.foreach(_.addTo(p))
    p
  }

  def changeShapeRule(shp: Shape, evenOdd: Boolean): Shape = {
    val p = new Path2D.Float(if (evenOdd) Path2D.WIND_EVEN_ODD else Path2D.WIND_NON_ZERO)
    val v = mkCmd(shp)
    v.foreach(_.addTo(p))
    p
  }

  private def mkShape(cmd: Vector[PathCmd]): Shape = {
    val p = new Path2D.Float()
    cmd.foreach(_.addTo(p))
    p
  }

  def difference(shp: Shape): Shape = {
    val v = mkCmd(shp)
    val itGroup0 = mkGroups(v)
//    val groups = itGroup0.toVector
//    println(s"difference size = ${groups.size}")
//    val itGroup = groups.iterator
    val itGroup = itGroup0
    if (itGroup.hasNext) return shp
    val a1 = new Area(mkShape(itGroup.next()))
    itGroup.foreach { cmd =>
      val a2 = mkShape(cmd)
      a1.subtract(new Area(a2))
    }
    a1
  }

  def lines(shp: Shape, n: Int): Shape = {
    val v   = mkCmd(shp)
    var x   = 0.0
    var y   = 0.0
    val v1  = v.flatMap { in =>
      val res = in.toLines(x, y, n)
      in match {
        case to: PathCmdTo =>
          x = to.endX
          y = to.endY
        case _ =>
      }
      res
    }
    mkShape(v1)
  }

  def shiftShape(name: String, shp: Shape, shift: Int): Shape = {
    val v = mkCmd(shp)

    import kollflitz.Ops._
    val itGroup = mkGroups(v)
    if (!itGroup.hasNext) return shp
    val head: Vector[PathCmd] = itGroup.next()

    val segm = head.groupWith {
      case (_, PathLine(_, _)) => false
      case _ => true
    } .toIndexedSeq

    // println("----SEGM----")
    // segm.foreach(println)

    // if (shift >= segm.size) println(s"WARNING: $name segm.size = ${segm.size}")

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
    val phase   = phase0.linLin(0, 1, -PiH, PiH).sin.linLin(-1, 1, 0, 1)
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
            // if (shiftA != 0 || shiftB != 0) println(s"($charA, $charB) -> ($shiftA, $shiftB)")
            // else if (!morphPairs.contains((charA, charB))) println(s"NOT: ($charA, $charB)")
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