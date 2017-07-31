/*
 *  Lucre.scala
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

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import edu.cmu.sphinx.linguist.dictionary.{Dictionary, Word}
import edu.cmu.sphinx.result

import scala.collection.{JavaConverters, breakOut}
import scala.language.implicitConversions

object Lucre {
  implicit object WordSerializer extends ImmutableSerializer[Word] {
    def write(w: Word, out: DataOutput): Unit =
      out.writeUTF(w.getSpelling)

    def read(in: DataInput): Word = {
      val spell     = in.readUTF()
      val isFiller  = spell == Dictionary.SILENCE_SPELLING // || spell == "<s>" || spell == "</s>"
      val w         = new Word(spell, new Array(0), isFiller)
      w
    }
  }

  object NodeDef {
    implicit def fromNode(n: result.Node): NodeDef =
      NodeDef(n.getId, n.getWord, n.getBeginTime, n.getEndTime,
        n.getForwardScore, n.getBackwardScore, n.getPosterior,
        Option(n.getBestPredecessor).map(_.getId), n.getViterbiScore)
  }
  case class NodeDef(id: String, word: Word, beginTime: Long, endTime: Long,
                     forward: Double, backward: Double, posterior: Double,
                     bestPred: Option[String], viterbi: Double)

  private[this] val StringOption = ImmutableSerializer.option[String]

  implicit object NodeSerializer extends ImmutableSerializer[NodeDef] {
    def write(n: NodeDef, out: DataOutput): Unit = {
      out.writeUTF        (n.id)
      WordSerializer.write(n.word, out)
      out.writeLong       (n.beginTime)
      out.writeLong       (n.endTime)
      out.writeDouble     (n.forward)
      out.writeDouble     (n.backward)
      out.writeDouble     (n.posterior)
      StringOption.write  (n.bestPred, out)
      out.writeDouble     (n.viterbi)
    }

    def read(in: DataInput): NodeDef = {
      val id    = in.readUTF()
      val word  = WordSerializer.read(in)
      val beginTime = in.readLong()
      val endTime   = in.readLong()
      val forward   = in.readDouble()
      val backward  = in.readDouble()
      val posterior = in.readDouble()
      val bestPred  = StringOption.read(in)
      val viterbi   = in.readDouble()

      NodeDef(id, word, beginTime = beginTime, endTime = endTime,
        forward = forward, backward = backward, posterior = posterior,
        bestPred = bestPred, viterbi = viterbi)
    }
  }

  object EdgeDef {
    implicit def fromEdge(e: result.Edge): EdgeDef =
      EdgeDef(e.getFromNode.getId, e.getToNode.getId, e.getAcousticScore, e.getLMScore)
  }
  case class EdgeDef(from: String, to: String, as: Double, lms: Double)

  implicit object EdgeSerializer extends ImmutableSerializer[EdgeDef] {

    def write(e: EdgeDef, out: DataOutput): Unit = {
      out.writeUTF   (e.from)
      out.writeUTF   (e.to  )
      out.writeDouble(e.as  )
      out.writeDouble(e.lms )
    }

    def read(in: DataInput): EdgeDef = {
      val from  = in.readUTF()
      val to    = in.readUTF()
      val as    = in.readDouble()
      val lms   = in.readDouble()
      val d = EdgeDef(from, to, as.toDouble, lms.toDouble)
      d
    }
  }

  implicit object LatticeSerializer extends ImmutableSerializer[result.Lattice] {
    def write(l: result.Lattice, out: DataOutput): Unit = {
      import JavaConverters._
      val initialId   = l.getInitialNode  .getId
      val terminalId  = l.getTerminalNode .getId
      val nodeSeq     = ImmutableSerializer.indexedSeq[NodeDef]
      val edgeSeq     = ImmutableSerializer.indexedSeq[EdgeDef]
      val nodeDefs    = l.getNodes.asScala.map(n => n: NodeDef)(breakOut): Vec[NodeDef]
      val edgeDefs    = l.getEdges.asScala.map(e => e: EdgeDef)(breakOut): Vec[EdgeDef]
      nodeSeq.write(nodeDefs, out)
      edgeSeq.write(edgeDefs, out)
      out.writeUTF(initialId)
      out.writeUTF(terminalId)
    }

    def read(in: DataInput): result.Lattice = {
      val nodeSeq     = ImmutableSerializer.indexedSeq[NodeDef]
      val edgeSeq     = ImmutableSerializer.indexedSeq[EdgeDef]
      val nodeDefs    = nodeSeq.read(in)
      val edgeDefs    = edgeSeq.read(in)
      val initialId   = in.readUTF()
      val terminalId  = in.readUTF()

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
  }
}