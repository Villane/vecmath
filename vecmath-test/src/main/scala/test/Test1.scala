package test

import org.villane.vecmath._
import org.villane.vecmath.Preamble._
import org.villane.vecmath.optimizer.sr

@sr object Test1 {
  @sr def compute(center: Vector2) = {
    val v1 = center * 2f - 2f
    v1
  }

  @sr def compute2(v1: Vector2, v2: Vector2) = {
    val v = v1 * 2f - v2 * 2f
    v
  }

  @sr def computeNormal(v: Vector2) = {
    val v1 = v × -2f
    v1
  }

  @sr def compute3(v1: Vector2, v2: Vector2) = {
    var v = v1 - v2
    v = v - v2
    v
  }

  @sr def compute4(v1: Vector2, v2: Vector2, b: Boolean) = {
    // should back out
    val v = if (b) v1 else v2
    v
  }

  @sr def identity(v: Vector2) = v

  @sr def selectScalar(v1: Vector2) = {
    val v = v1
    val b = v.x + 1
    Vector2(b + 1, v.y)
  }

  @sr def selectUnary(v1: Vector2) = {
    val v = v1
    val b = v.abs + 1
    Vector2(b.x, v.y)
  }

  @sr def selectNormalize(v1: Vector2) = {
    var v = v1
    val b = v.unit
    val c = v.unit
    // TODO this is currently pretty bad: does (v1 + 2).unit twice!!!
    v = (v1 + 2).unit
    val d = v.unit
    d
  }

  @sr def selectLen(v1: Vector2) = {
    var v = v1
    val l = v.length
    val ls = v.lengthSquared
    v
  }

  @sr def floatExt(v1: Vector2) = {
    var v = 2f * v1
    v
  }

  def main(args: Array[String]) = {
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
