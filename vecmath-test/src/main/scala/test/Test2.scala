package test

import org.villane.vecmath._
import org.villane.vecmath.Preamble._
import org.villane.vecmath.optimizer.sr

@sr object Test2 {
  def polarVector() = {
    val v = Vector2.polar(2, 20f)
    v
  }

  def rotMatrix() = {
    val m = Matrix22.rotation(2)
    m
  }

  def computePos(xf: Transform2, local: Vector2) = {
    val wp = xf.rot ** local
    wp.length
  }
}
