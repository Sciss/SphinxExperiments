/*
 *  Json.scala
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

import de.sciss.play.json.Formats
import edu.cmu.sphinx.linguist.dictionary.{Dictionary, Word}
import edu.cmu.sphinx.result
import play.api.libs.json.{Format, JsArray, JsError, JsNumber, JsObject, JsResult, JsString, JsSuccess, JsValue}

import scala.collection.{JavaConverters, breakOut}
import scala.language.implicitConversions

object Json {
  //  implicit object UnitFormat extends Format[acoustic.Unit] {
  //    def reads(json: JsValue): JsResult[acoustic.Unit] = ...
  //
  //    def writes(u: acoustic.Unit): JsValue = {
  //      // XXX TODO : require(u.getContext == null, "context not supported")
  //      JsObject(Seq(
  //        "name"  -> JsString (u.getName),
  //        // "fill"  -> JsBoolean(u.isFiller),  -- only "SIL" is true
  //        "id"    -> JsNumber (u.getBaseID)
  //      ))
  //    }
  //  }

  implicit object WordFormat extends Format[Word] {
    def reads(json: JsValue): JsResult[Word] = json match {
      case JsString(spell) =>
        val isFiller = spell == Dictionary.SILENCE_SPELLING // || spell == "<s>" || spell == "</s>"
        val w = new Word(spell, new Array(0), isFiller)
        JsSuccess(w)

      case _ => JsError("Unexpected Json value")
    }

    def writes(w: Word): JsValue = {
      // We should be able to reconstruct pronunciations from dict.
      // It's not that this instance denotes a particular pronunciation.
      JsString(w.getSpelling)
//      JsObject(Seq(
//        "spell" -> JsString(w.getSpelling),
//        "pron"  -> JsArray(w.getPronunciations.map(PronunciationFormat.writes))
//      ))
    }
  }

  // private[this] val WordOptionFormat = Formats.OptionFormat[Word]

  //  implicit object PronunciationFormat extends Format[Pronunciation] {
  //    def reads(json: JsValue): JsResult[Pronunciation] = ...
  //
  //    def writes(p: Pronunciation): JsValue = {
  //      JsArray(p.getUnits.map(u => JsString(u.getName)))
  ////      JsObject(Seq(
  ////        "units" -> JsArray(p.getUnits.map(UnitFormat.writes))
  ////        // "tag"   -> JsString(p.getTag), -- always null
  ////        // "prob"  -> JsNumber(p.getProbability.toDouble) -- always 1.0
  ////        // "word" -> p.getWord
  ////      ))
  //    }
  //  }

  object NodeDef {
    implicit def fromNode(n: result.Node): NodeDef =
      NodeDef(n.getId, n.getWord, n.getBeginTime, n.getEndTime,
        n.getForwardScore, n.getBackwardScore, n.getPosterior,
        Option(n.getBestPredecessor).map(_.getId), n.getViterbiScore)
  }
  case class NodeDef(id: String, word: Word, beginTime: Long, endTime: Long,
                     forward: Double, backward: Double, posterior: Double,
                     bestPred: Option[String], viterbi: Double)

  private[this] val StringOption = Formats.OptionFormat[String]

  implicit object NodeFormat extends Format[NodeDef] {
    def writes(n: NodeDef): JsValue =
      JsObject(Seq(
        "id"    -> JsString(n.id),
        "word"  -> WordFormat.writes(n.word),
        "begin" -> JsNumber(n.beginTime),
        "end"   -> JsNumber(n.endTime),
        "fwd"   -> JsNumber(n.forward),
        "bwd"   -> JsNumber(n.backward),
        "post"  -> JsNumber(n.posterior),
        "pred"  -> StringOption.writes(n.bestPred),
        "vit"   -> JsNumber(n.viterbi)
      ))

    def reads(json: JsValue): JsResult[NodeDef] = json match {
      case JsObject(Seq(
        ("id"   , JsString(id)),
        ("word" , wordJ),
        ("begin", JsNumber(beginTime)),
        ("end"  , JsNumber(endTime)),
        ("fwd"  , JsNumber(forward)),
        ("bwd"  , JsNumber(backward)),
        ("post" , JsNumber(posterior)),
        ("pred" , bestPredJ),
        ("vit"  , JsNumber(viterbi))
      )) =>

        for {
          word     <- WordFormat.reads(wordJ)
          bestPred <- StringOption.reads(bestPredJ)
        } yield {
          NodeDef(id, word, beginTime = beginTime.toLong, endTime = endTime.toLong,
            forward = forward.toDouble, backward = backward.toDouble, posterior = posterior.toDouble,
            bestPred = bestPred, viterbi = viterbi.toDouble)
        }

      case _ => JsError("Unexpected Json value")
    }
  }
  
  object EdgeDef {
    implicit def fromEdge(e: result.Edge): EdgeDef =
      EdgeDef(e.getFromNode.getId, e.getToNode.getId, e.getAcousticScore, e.getLMScore)
  }
  case class EdgeDef(from: String, to: String, as: Double, lms: Double)

  implicit object EdgeFormat extends Format[EdgeDef] {
    def reads(json: JsValue): JsResult[EdgeDef] = json match {
      case JsObject(Seq(
          ("from" , JsString(from)),
          ("to"   , JsString(to  )),
          ("as"   , JsNumber(as  )),
          ("lms"  , JsNumber(lms ))
        )) =>

        val d = EdgeDef(from, to, as.toDouble, lms.toDouble)
        JsSuccess(d)

      case _ => JsError("Unexpected Json value")
    }

    def writes(e: EdgeDef): JsValue =
      JsObject(Seq(
        "from"  -> JsString(e.from),
        "to"    -> JsString(e.to  ),
        "as"    -> JsNumber(e.as  ),
        "lms"   -> JsNumber(e.lms )
      ))
  }

  implicit object LatticeFormat extends Format[result.Lattice] {
    def reads(json: JsValue): JsResult[result.Lattice] = json match {
      case JsObject(Seq(
          ("nodes", nodesJ),
          ("edges", edgesJ),
          ("init" , JsString(initialId)),
          ("term" , JsString(terminalId))
        )) =>

        for {
          nodeDefs  <- Formats.VecFormat[NodeDef].reads(nodesJ)
          edgeDefs  <- Formats.VecFormat[EdgeDef].reads(edgesJ)
        } yield {

          val l = new MyLattice // result.Lattice()

          val nodes0: Map[String, (result.Node, Option[String])] = nodeDefs.map { d =>
            val n = l.addNode(d.id, d.word, d.beginTime, d.endTime)
            n.setForwardScore (d.forward)
            n.setBackwardScore(d.backward)
            n.setPosterior    (d.posterior)
            n.setViterbiScore (d.viterbi)
            d.id -> ((n, d.bestPred))
          } (breakOut)

          val nodes: Map[String, result.Node] = nodes0.map { case (id, (n, predId)) =>
            predId.foreach { id =>
              val pred = nodes0(id)._1
              n.setBestPredecessor(pred)
            }
            id -> n
          }

          edgeDefs.foreach { d =>
            val from = nodes(d.from)
            val to   = nodes(d.to  )
            l.addEdge(from, to, d.as, d.lms)
          }

          val initNode = nodes(initialId )
          val termNode = nodes(terminalId)
          l.setInitialNode (initNode)
          l.setTerminalNode(termNode)
          l
        }

      case _ => JsError("Unexpected Json value")
    }

    def writes(l: result.Lattice): JsValue = {
      import JavaConverters._

      val initialId   = l.getInitialNode  .getId
      val terminalId  = l.getTerminalNode .getId
      val nodes       = l.getNodes.asScala.map(n => NodeFormat.writes(n))(breakOut): Seq[JsValue]
      val edges       = l.getEdges.asScala.map(e => EdgeFormat.writes(e))(breakOut): Seq[JsValue]

      JsObject(Seq(
        "nodes" -> JsArray(nodes),
        "edges" -> JsArray(edges),
        "init"  -> JsString(initialId),
        "term"  -> JsString(terminalId)
      ))
    }
  }
}