package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait ScalarScalarizer { self: VecMathTransformer =>
  import global._
  import definitions._

  val ToFloat = newTermName("toFloat")

  trait ScalarTransformer { self: VMTransformer =>

      /*case Scalar() => tree match {
        case Literal(c @ Constant(_)) => super.transform(Literal(c.floatValue))
        case _ => typed(Select(super.transform(tree), ToFloat))
      }*/

    def scalarizeV2Length(v: Tree) = v match {
      case Ident(name) if scope.inlinedVar(name).isDefined =>
        val iv = scope.v2(name)
        val ref = expecting match {
          case Escaping(_,_) => iv.lengthN.isDefined && !iv.lengthDirty
          case _ => true
        }
        if (ref) {
          iv.forceLengthCaching
          typed(Ident(iv.lengthN.get))
        } else
          MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))
      /*case t if t.symbol != null && t.symbol.isStable =>
        val iv = ANONYMOUS*/
      case _=>
        MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))
        // worst case, we create a temp vector
        // UnOp(deScalar(v), Length)
    }

    def scalarizeV2Theta(v: Tree) =
      MathFun(VecMath, ATan2, expectY(xf(v)), expectX(xf(v)))

    def scalarizeV2LengthSquared(v: Tree) = {
      val x = BinOp(expectX(xf(v)), nme.MUL, expectX(xf(v)))
      val y = BinOp(expectY(xf(v)), nme.MUL, expectY(xf(v)))              
      BinOp(x, nme.ADD, y)
    }

    def scalarizeV2DotV2(a: Tree, v: Tree) = {
      val x = BinOp(expectX(xf(a)), nme.MUL, expectX(xf(v)))
      val y = BinOp(expectY(xf(a)), nme.MUL, expectY(xf(v)))
      BinOp(x, nme.ADD, y)
    }

    def scalarizeV2CrossV2(a: Tree, v: Tree) = {
      val x = BinOp(expectX(xf(a)), nme.MUL, expectY(xf(v)))
      val y = BinOp(expectY(xf(a)), nme.MUL, expectX(xf(v)))
      BinOp(x, nme.SUB, y)
    }

  }

}
