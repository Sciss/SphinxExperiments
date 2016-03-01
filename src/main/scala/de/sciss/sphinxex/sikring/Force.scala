package de.sciss.sphinxex.sikring

trait Force {
  def eval(edge: Edge): (DoublePoint2D, DoublePoint2D)
}