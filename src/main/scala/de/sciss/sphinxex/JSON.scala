/*
 *  JSON.scala
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

import edu.cmu.sphinx.linguist.acoustic
import edu.cmu.sphinx.linguist.dictionary.{Pronunciation, Word}
import edu.cmu.sphinx.result
import edu.cmu.sphinx.result.Edge
import play.api.libs.json.{Format, JsArray, JsBoolean, JsNumber, JsObject, JsResult, JsString, JsValue}

import scala.collection.{JavaConverters, breakOut}

object JSON {
  implicit object UnitFormat extends Format[acoustic.Unit] {
    def reads(json: JsValue): JsResult[acoustic.Unit] = ???

    def writes(u: acoustic.Unit): JsValue = {
      require(u.getContext == null, "context not supported")
      JsObject(Seq(
        "name"    -> JsString (u.getName),
        "filler"  -> JsBoolean(u.isFiller),
        "id"      -> JsNumber (u.getBaseID)
      ))
    }
  }

  implicit object WordFormat extends Format[Word] {
    def reads(json: JsValue): JsResult[Word] = ???

    def writes(w: Word): JsValue =
      JsObject(Seq(
        "spelling"        -> JsString(w.getSpelling),
        "pronunciations"  -> JsArray (w.getPronunciations.map(PronunciationFormat.writes))
      ))
  }

  // private[this] val WordOptionFormat = Formats.OptionFormat[Word]

  implicit object PronunciationFormat extends Format[Pronunciation] {
    def reads(json: JsValue): JsResult[Pronunciation] = ???

    def writes(p: Pronunciation): JsValue =
      JsObject(Seq(
        "units" -> JsArray (p.getUnits.map(UnitFormat.writes)),
        "tag"   -> JsString(p.getTag),
        "prob"  -> JsNumber(p.getProbability.toDouble)
        // "word" -> p.getWord
      ))
  }

  implicit object NodeFormat extends Format[result.Node] {
    def reads(json: JsValue): JsResult[result.Node] = ???

    def writes(n: result.Node): JsValue =
      JsObject(Seq(
        "id"    -> JsString(n.getId),
        "word"  -> WordFormat.writes(n.getWord),
        "begin" -> JsNumber(n.getBeginTime),
        "end"   -> JsNumber(n.getEndTime)
      ))
  }

  implicit object EdgeFormat extends Format[result.Edge] {
    def reads(json: JsValue): JsResult[Edge] = ???

    def writes(e: Edge): JsValue =
      JsObject(Seq(
        "from"        -> JsString(e.getFromNode.getId),
        "to"          -> JsString(e.getToNode  .getId),
        "acoustical"  -> JsNumber(e.getAcousticScore),
        "language"    -> JsNumber(e.getLMScore)
      ))
  }

  implicit object LatticeFormat extends Format[result.Lattice] {
    def reads(json: JsValue): JsResult[result.Lattice] = ???

    def writes(l: result.Lattice): JsValue = {
      import JavaConverters._

      val initialId   = l.getInitialNode  .getId
      val terminalId  = l.getTerminalNode .getId

      val nodes = l.getNodes.asScala.map(NodeFormat.writes)(breakOut): Seq[JsValue]
      val edges = l.getEdges.asScala.map(EdgeFormat.writes)(breakOut): Seq[JsValue]

      JsObject(Seq(
        "nodes"     -> JsArray(nodes),
        "edges"     -> JsArray(edges),
        "initial"   -> JsString(initialId),
        "terminal"  -> JsString(terminalId)
      ))
    }
  }
}