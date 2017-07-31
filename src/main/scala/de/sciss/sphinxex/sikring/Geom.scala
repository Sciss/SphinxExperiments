/*
 *  Geom.scala
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

object DoublePoint2D {
  final val Zero = DoublePoint2D(0, 0)
}
final case class DoublePoint2D(x: Double, y: Double) {
  def + (that: DoublePoint2D): DoublePoint2D = DoublePoint2D(this.x + that.x, this.y + that.y)
}

final case class DoubleRectangle2D(x: Double, y: Double, width: Double, height: Double) {
  /** Center x */
  def cx: Double = x + width /2
  /** Center y */
  def cy: Double = y + height/2
}