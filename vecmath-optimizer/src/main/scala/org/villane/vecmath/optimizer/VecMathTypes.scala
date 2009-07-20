package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait VecMathTypes { self: VecMathOptimizer =>
  import global._
  import definitions._             // standard classes and methods

  // Annotation to mark classes and methods as optimizable
  val srAnnot = definitions.getClass("org.villane.vecmath.optimizer.sr")
  val nosrAnnot = definitions.getClass("org.villane.vecmath.optimizer.nosr")

  // Math function containers
  val StdMath = Ident(getModule("scala.Math"))
  val VecMath = Ident(getModule("org.villane.vecmath.Preamble"))

  // Vector, Matrix, Transform types
  val FT = FloatClass.tpe
  val FEXTT = definitions.getClass("org.villane.vecmath.FloatExtensions").tpe
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

  object T2 {
    def apply() = TypeTree(T2T)
    def unapply(tr: Tree): Boolean = isT2(tr.tpe)
  }

}
