package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait Vector2Scalarizer { self: VecMathTransformer =>
  import global._
  import definitions._

  // Untyped
  object UTBinOp {
    def apply(l: Tree, op: Name, r: Tree) = Apply(Select(l, op), List(r))
    def unapply(tr: Tree) = tr match {
      case Apply(Select(l, op), List(r)) => Some(l, op, r)
      case _ => None
    }
  }

  // Untyped
  object UTUnOp {
    def apply(l: Tree, op: Name) = Select(l, op)
    def unapply(tr: Tree) = tr match {
      case Select(l, op) => Some(l, op)
      case _ => None
    }
  }

  /**
   * Turns a Vector2 expression into a scalar expression
   */
  trait V2Scalarizer { self: VMTransformer =>
    object BinOp {
      def apply(l: Tree, op: Name, r: Tree) = typed(Apply(Select(l, op), List(r)))
      def unapply(tr: Tree) = tr match {
        case Apply(Select(l, op), List(r)) => Some(l, op, r)
        case _ => None
      }
    }

    object UnOp {
      def apply(l: Tree, op: Name) = typed(Select(l, op))
      def unapply(tr: Tree) = tr match {
        case Select(l, op) => Some(l, op)
        case _ => None
      }
    }

    object MathFun {
      def apply(math: Ident, op: Name, args: Tree*) = typed(Apply(Select(math, op), args.toList))
      def unapplySeq(tr: Tree) = tr match {
        case Apply(Select(math, op), args) => Some(math, op, args.toSeq)
        case _ => None
      }
    }

    object NewObj {
      def apply(tpt: TypeTree, args: Tree*) = typed(Apply(Select(New(tpt), nme.CONSTRUCTOR), args.toList))
    }

    def scalarizeM22MulV2(m: Tree, v: Tree) = expecting match {
      //X=a11 * v.x + a12 * v.y
      case Scalarized(V2T, X) => BinOp(
        BinOp(expectA11(xf(m)), nme.MUL, xf(v)),
        nme.ADD,
        BinOp(expectA12(xf(m)), nme.MUL, expectY(xf(v)))
      )
      //Y=a21 * v.x + a22 * v.y
      case Scalarized(V2T, Y) => BinOp(
        BinOp(expectA21(xf(m)), nme.MUL, expectX(xf(v))),
        nme.ADD,
        BinOp(expectA22(xf(m)), nme.MUL, xf(v))
      )
    }

    def scalarizeM22MulTransV2(m: Tree, v: Tree) = expecting match {
      //X=a11 * v.x + a21 * v.y
      case Scalarized(V2T, X) => BinOp(
        BinOp(expectA11(xf(m)), nme.MUL, xf(v)),
        nme.ADD,
        BinOp(expectA21(xf(m)), nme.MUL, expectY(xf(v)))
      )
      //Y=a12 * v.x + a22 * v.y
      case Scalarized(V2T, Y) => BinOp(
        BinOp(expectA12(xf(m)), nme.MUL, expectX(xf(v))),
        nme.ADD,
        BinOp(expectA22(xf(m)), nme.MUL, xf(v))
      )
    }
  }

}
