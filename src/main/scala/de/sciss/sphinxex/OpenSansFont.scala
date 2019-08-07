/*
 *  MyFont.scala
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

import java.awt.Font

trait MyFont {
  def name: String

  private[this] lazy val _initFont: Font = {
    val url = getClass.getResource(s"/$name.ttf")
    require(url != null)
    val is = url.openStream()
    val res = Font.createFont(Font.TRUETYPE_FONT, is)
    is.close()
    res
  }

  private[this] var _font: Font = _

  def apply(): Font = {
    if (_font == null) _font = _initFont
    _font
  }

  def apply(size: Float): Font = apply().deriveFont(size)
}

object OpenSansFont extends MyFont {
  val name = "OpenSans-CondLight"
}

object GulimFont extends MyFont {
  val name = "Gulim"
}
