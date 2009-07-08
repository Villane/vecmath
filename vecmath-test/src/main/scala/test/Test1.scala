package test

import org.villane.vecmath._
import org.villane.vecmath.Preamble._
import org.villane.vecmath.optimizer.sr
import org.villane.vecmath.optimizer.nosr

@sr object Test1 {
  def compute(center: Vector2) = {
    val v1 = center * 2f * 1.3f
    v1
  }

  def compute2(v1: Vector2, v2: Vector2) = {
    val v = v1 * 2f - v2 * 2f
    v
  }

  def computeNormal(v: Vector2) = {
    val v1 = v × -2f
    v1
  }

  def computeNormal2(v1: Vector2, v2: Vector2) = {
    val v = (v1 - v2) × -2f
    v
  }

  def compute3(v1: Vector2, v2: Vector2) = {
    var v = v1 - v2
    v = v - v2
    v
  }

  def compute4(v1: Vector2, v2: Vector2, b: Boolean) = {
    // should back out
    val v = if (b) v1 else v2
    v
  }

  def identity(v: Vector2) = v

  def selectScalar(v1: Vector2) = {
    val v = v1
    val b = v.x + 1
    Vector2(b + 1, v.y)
  }

  def selectUnary(v1: Vector2) = {
    val v = v1
    val b = v.abs * 1.2f
    Vector2(b.x, v.y)
  }

  def selectNormalize(v1: Vector2) = {
    var v = v1
    val b = v.unit
    val c = v.unit
    // TODO this is currently pretty bad: does (v1 + 2).unit twice!!!
    v = (v1 - Vector2(2,2)).unit
    val d = v.unit
    d
  }

  def selectLen(v1: Vector2) = {
    var v = -v1
    val l = v.length
    val ls = v.lengthSquared
    v
  }

  def floatExt(v1: Vector2) = {
    var v = 2f * v1 + Vector2.YUnit
    var v2 = (v1 dot v) * v
    v
  }

  def complexExpr(v1: Vector2, v2: Vector2, v3: Vector2) = {
    var vd = ((v2 - v1) dot v3) * Vector2(1, 3)
    var vx = ((v2 - v1) cross v3) * Vector2(1, 3)
    vx - vd
  }

  class V2Holder(val v1: Vector2, val v2: Vector2)

  def holder(h: V2Holder) = {
    var v = -(h.v2 - h.v1)
    v
  }

  @nosr def main(args: Array[String]) = {
    val res = List(
      compute(5, 5),
      compute2((5, 5), (6, 6)),
      computeNormal(1,3),
      compute3((7, 3),(6, 2)),
      compute4((1, 1),(2, 2),true),
      identity((31,31)),
      selectScalar(1, 1),
      selectUnary(-1, -1),
      selectNormalize(2, 4),
      selectLen(2, 4)
    )
    println(res mkString "\n")
  }
}
