package org.villane.vecmath.optimizer

trait VecMathOperations { self: VecMathTransformer =>
  import global._
  import definitions._             // standard classes and methods

  // Math
  val Sqrt = newTermName("sqrt")
  val Sin = newTermName("sin")
  val Cos = newTermName("cos")
  val ATan2 = newTermName("atan2")
  val FloatExt = newTermName("floatToFloatExtensions")

  // Vector2
  val Zero = newTermName("Zero")
  val One = newTermName("One")
  val XUnit = newTermName("XUnit")
  val YUnit = newTermName("YUnit")
  val Polar = newTermName("polar")

  val X = newTermName("x")
  val Y = newTermName("y")
  def ortho(coord: Name) = if (coord == X) Y else X

  // Matrix22
  val A11 = newTermName("a11")
  val A12 = newTermName("a12")
  val A21 = newTermName("a21")
  val A22 = newTermName("a22")
  val Col1 = newTermName("col1")
  val Col2 = newTermName("col2")

  var scalarComponents = Set(X, Y, A11, A12, A21, A22)
  def isScalarComponent(name: Name) = scalarComponents contains name
  object ScalarComponent {
    def unapply(name: Name) = isScalarComponent(name)
  }

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

}
