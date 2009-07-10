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

  def exprLength(v1: Vector2, v2: Vector2) = {
    val n = (v1 - v2).length
    n
  }

  case class MutHolder(var v: Vector2)

  def mutableExprLength(v1: Vector2, h: MutHolder) = {
    val n = (v1 - h.v).length
    h.v = Vector2(7,7)
    val m = (v1 - h.v).length
    n - m
  }

  def computePos(xf: Transform2, local: Vector2) = {
    val wp = xf.rot ** local
    wp.length
  }

  def main(args: Array[String]) = {
    println(mutableExprLength(Vector2(1,1), MutHolder(Vector2(2,2))))
  }
}
