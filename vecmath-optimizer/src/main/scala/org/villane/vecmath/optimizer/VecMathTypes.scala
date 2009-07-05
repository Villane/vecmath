package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait VecMathTypes { self: VecMathTransformer =>
  import global._
  import definitions._             // standard classes and methods

  // Annotation to mark classes and methods as optimizable
  val srAnnot = definitions.getClass("org.villane.vecmath.optimizer.sr")

  // Math function containers
  val StdMath = Ident(getModule("scala.Math"))
  val VecMath = Ident(getModule("org.villane.vecmath.Preamble"))

  // Vector, Matrix, Transform types
  val FT = FloatClass.tpe
  val NativeScalar = FT
  val FEXTT = definitions.getClass("org.villane.vecmath.FloatExtensions").tpe
  val V2T = definitions.getClass("org.villane.vecmath.Vector2").tpe
  val V2O = definitions.getModule("org.villane.vecmath.Vector2")
  val M22T = definitions.getClass("org.villane.vecmath.Matrix22").tpe
  val T2T = definitions.getClass("org.villane.vecmath.Transform2").tpe

  // isScalar = actually, is implicitly convertible to "native" scalar (Float)
  def isScalar(tpe: Type) = isF(tpe) |
    // NOT!!! isOfClass(tpe, DoubleClass) |
    isOfClass(tpe, IntClass) |
    isOfClass(tpe, ShortClass) |
    isOfClass(tpe, LongClass) |
    isOfClass(tpe, ByteClass)

  def isOfClass(tpe: Type, cls: Symbol) = tpe == cls.tpe || tpe.widen == cls.tpe

  def isF(tpe: Type) = tpe == FT || tpe.widen == FT
  def isFEXT(tpe: Type) = tpe == FEXTT || tpe.widen == FEXTT
  def isV2(tpe: Type) = tpe == V2T || tpe.widen == V2T // TODO tpe.normalize?
  def isM22(tpe: Type) = tpe == M22T || tpe.widen == M22T
  def isT2(tpe: Type) = tpe == T2T || tpe.widen == T2T

  object FEXT {
    def unapply(tr: Tree) = isFEXT(tr.tpe)
  }

  object Scalar {
    def unapply(tr: Tree) = isScalar(tr.tpe)
  }

  object F {
    def apply() = TypeTree(FT)
    def unapply(tr: Tree) = isF(tr.tpe)
  }

  object V2 {
    def apply() = TypeTree(V2T)
    def unapply(tr: Tree): Boolean = isV2(tr.tpe)
    def unapply(tpe: Type): Boolean = isV2(tpe)
  }

  object V2Ob {
    def apply() = V2O
    def unapply(tr: Tree): Boolean = tr.tpe == V2O.tpe || tr.tpe.widen == V2O.tpe
    def unapply(tpe: Type): Boolean = tpe == V2O.tpe || tpe.widen == V2O.tpe
  }

  object M22 {
    def apply() = TypeTree(M22T)
    def unapply(tr: Tree): Boolean = isM22(tr.tpe)
  }

}
