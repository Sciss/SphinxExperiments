/*
 *  package.scala
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

package de.sciss

import java.awt.Shape

import de.sciss.processor.ProcessorLike
import de.sciss.sphinxex.sikring.Vertex

import scala.concurrent.ExecutionContext

package object sphinxex {
  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq

  implicit final class VecOps[A](private val in: Vec[A]) extends AnyVal {
    def removeAt(index: Int         ): Vec[A] = in.patch(index, Nil        , 1)
    def insert  (index: Int, elem: A): Vec[A] = in.patch(index, elem :: Nil, 0)
  }

  implicit final class ShapeOps(private val in: Shape) extends AnyVal {
    def isEmpty  : Boolean = in == Vertex.EmptyShape
    def isDefined: Boolean = !isEmpty
  }

  def mkBlockTread(): AnyRef = {
    val sync = new AnyRef
    val t = new Thread {
      override def run(): Unit = {
        sync.synchronized(sync.wait())
        Thread.sleep(100)
      }
    }
    t.start()
    sync
  }

  def waitForProcessor(p: ProcessorLike[Any, Any])(implicit executionContext: ExecutionContext): Unit = {
    val sync = mkBlockTread()
    p.onComplete(_ => sync.synchronized(sync.notify()))
  }

  def exitWithProcessor(p: ProcessorLike[Any, Any])(implicit executionContext: ExecutionContext): Unit = {
    waitForProcessor(p)
    p.foreach {_ =>
      Thread.sleep(200)
      sys.exit()
    }
  }

  implicit val executionContext: ExecutionContext /* Executor */ = ExecutionContext.Implicits.global
}
