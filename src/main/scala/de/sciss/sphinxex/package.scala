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

import de.sciss.processor.ProcessorLike

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

package object sphinxex {
  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq

  implicit final class VecOps[A](private val in: Vec[A]) extends AnyVal {
    def removeAt(index: Int         ): Vec[A] = in.patch(index, Nil        , 1)
    def insert  (index: Int, elem: A): Vec[A] = in.patch(index, elem :: Nil, 0)
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

  def waitForProcessor(p: ProcessorLike[Any, Any])(implicit exec: ExecutionContext): Unit = {
    val sync = mkBlockTread()
    p.onComplete {
      case _ => sync.synchronized(sync.notify())
    }
  }

  def exitWithProcessor(p: ProcessorLike[Any, Any])(implicit exec: ExecutionContext): Unit = {
    waitForProcessor(p)
    p.onSuccess {
      case _ =>
        Thread.sleep(200)
        sys.exit()
    }
  }

  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global
}
