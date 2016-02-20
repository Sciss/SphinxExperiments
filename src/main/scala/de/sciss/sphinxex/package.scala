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

import de.sciss.processor.{Processor, ProcessorLike}

import scala.concurrent.{ExecutionContextExecutor, ExecutionContext}

package object sphinxex {
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
