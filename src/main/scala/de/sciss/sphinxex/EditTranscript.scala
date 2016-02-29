/*
 *  EditTranscript.scala
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

import scala.annotation.tailrec

object EditTranscript {
  final val Copy        = '-'
  final val Substitute  = 'S'
  final val Insert      = 'I'
  final val Delete      = 'D'

  /** Calculates the edit transcript from one string `a` to another `b`,
    * trying to minimise the cost of operations.
    *
    * @return the transcript string where '-' is copy, 'D' is delete, 'I' is insert, 'S' is substitute
    */
  def apply(a: String, b: String): String = {
    val aLen  = a.length
    val bLen  = b.length
    val dist : Array[Array[Int ]] = Array.ofDim(aLen + 1, bLen + 1)
    val trans: Array[Array[Char]] = Array.ofDim(aLen + 1, bLen + 1)

    def min(a: Int, b: Int, c: Int): Int = math.min(a, math.min(b, c))

    // initialise matrices
    for (i <- 0 to a.length) {
      dist (i)(0) = i
      trans(i)(0) = 'D'
    }
    for (i <- 0 to bLen) {
      dist (0)(i) = i
      trans(0)(i) = 'I'
    }

    for (i <- 1 to aLen) {
      for (j <- 1 to bLen) {
        val same  = a.charAt(i - 1) == b.charAt(j - 1)
        val cIns  = dist(i    )(j - 1) + 1
        val cDel  = dist(i - 1)(j    ) + 1
        val cAlt  = dist(i - 1)(j - 1) + (if (same) 0 else 1)

        val cBest = min(cIns, cDel, cAlt)
        dist(i)(j) = cBest
        val edit =
          if      (cBest == cIns) 'I'
          else if (cBest == cDel) 'D'
          else if (same)          '-'
          else                    'S'

        trans(i)(j) = edit
      }
    }

    // go back in matrix to create transcript
    @tailrec
    def trace(i: Int, j: Int, res: List[Char]): List[Char] =
      if (i == 0 && j == 0) res else {
        val edit  = trans(i)(j)
        val res1  = edit :: res
        val isAlt = edit == 'S' || edit == '-'
        val i1    = if (i > 0 && (isAlt || edit == 'D')) i - 1 else i
        val j1    = if (j > 0 && (isAlt || edit == 'I')) j - 1 else j
        trace(i1, j1, res1)
      }

    val st = trace(aLen, bLen, Nil)
    st.mkString
  }
}