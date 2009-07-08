package org.villane.vecmath.optimizer

trait VecMathOperations { self: VecMathOptimizer =>
  import global._
  import definitions._

  // Constants
  val Zero = newTermName("Zero")
  val One = newTermName("One")

  // Math
  val Sqrt = newTermName("sqrt")
  val Sin = newTermName("sin")
  val Cos = newTermName("cos")
  val ATan2 = newTermName("atan2")
  val FloatExt = newTermName("floatToFloatExtensions")

  // V2 Unary
  val Normal = newTermName("normal")
  val Swap = newTermName("swap")
  val Abs = newTermName("abs")
  val NormalizeOps = Set(newTermName("normalize"), newTermName("unit"))
  val ThetaOps = Set(newTermName("θ"), newTermName("theta"))
  // V2 Unary returning scalar
  val Length = newTermName("length")
  val LengthSqr = newTermName("lengthSquared")
  // V2 Binary
  val SimpleV2FOps = Set(nme.MUL, nme.DIV)
  val SimpleV2V2Ops = Set(nme.ADD, nme.SUB)
  val CrossOp = Set(newTermName("$u00D7"), newTermName("cross"))
  val DotOp = Set(newTermName("$u2219"), newTermName("dot"))

  object Normalize { def unapply(op: Name) = NormalizeOps contains op }
  object Theta { def unapply(op: Name) = ThetaOps contains op }
  object SimpleV2F { def unapply(op: Name) = SimpleV2FOps contains op }
  object SimpleV2V2 { def unapply(op: Name) = SimpleV2V2Ops contains op }
  object Cross { def unapply(op: Name) = CrossOp contains op }
  object Dot { def unapply(op: Name) = DotOp contains op }

  // Matrix2 binary
  val MulTrans  = encode("**")

  // Transform2
  val Pos = "pos" // position
  val Rot = "rot" // rotation
}
