package de.sciss.sphinxex

import edu.cmu.sphinx.linguist.dictionary.Word
import edu.cmu.sphinx.result.{Node, Lattice}

class MyLattice extends Lattice {
  /** Makes this method public. */
  override def addNode(id: String, word: Word, beginTime: Long, endTime: Long): Node =
    super.addNode(id, word, beginTime, endTime)
}
