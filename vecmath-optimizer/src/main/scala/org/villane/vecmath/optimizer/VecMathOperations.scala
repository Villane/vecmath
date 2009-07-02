package org.villane.vecmath.optimizer

trait VecMathOperations { self: VecMathTransformer =>
  import global._
  import definitions._             // standard classes and methods

  // Math
  val Sqrt = newTermName("sqrt")
  val Sin = newTermName("sin")
  val Cos = newTermName("cos")
  val FloatExt = newTermName("floatToFloatExtensions")

  // Vector2
  val Zero = newTermName("Zero")
  val One = newTermName("One")
  val XUnit = newTermName("XUnit")
  val YUnit = newTermName("YUnit")
  val X = newTermName("x")
  val Y = newTermName("y")
  def ortho(coord: Name) = if (coord == X) Y else X

  // V2 Unary
  val Abs = newTermName("abs")
  val NormalizeOps = Set(newTermName("normalize"), newTermName("unit"))
  // V2 Unary returning scalar
  val Length = newTermName("length")
  val LengthSqr = newTermName("lengthSquared")
  // V2 Binary
  val SimpleV2FOps = Set(nme.ADD, nme.SUB, nme.MUL, nme.DIV)
  val SimpleV2V2Ops = Set(nme.ADD, nme.SUB)
  val CrossOp = Set(newTermName("$u00D7"), newTermName("cross"))
  val DotOp = Set(newTermName("$u2219"), newTermName("dot"))

  object Normalize { def unapply(op: Name) = NormalizeOps contains op }
  object SimpleV2F { def unapply(op: Name) = SimpleV2FOps contains op }
  object SimpleV2V2 { def unapply(op: Name) = SimpleV2V2Ops contains op }
  object Cross { def unapply(op: Name) = CrossOp contains op }
  object Dot { def unapply(op: Name) = DotOp contains op }
}
