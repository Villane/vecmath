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

  /**
   * Applies scalarization changes to a Scalar expression
   */
  class ScalarScalarizer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def typed(tr: Tree) = localTyper.typed(tr)
    def typed(tr: Tree, mode: Int, tp: Type) = localTyper.typed(tr, mode, tp)

    override def transform(tree: Tree): Tree = tree match {
      // TODO add @V2
      case Select(v, X) => typed(scalar(v, X))
      case Select(v, Y) => typed(scalar(v, Y))
      case Select(v, Length) => scalarizeLengthV2(v)
      case Select(v, LengthSqr) => scalarizeLengthSqrV2(v)
      case Select(v, op) =>
        println("SCALAR UnaryOP!" + tree)
        typed(Select(transform(v), op))
      case Apply(Select(v0, op), List(a0)) =>
        val v = transform(v0)
        val a = transform(a0)
        println("SCALAR BinOp: " + (v, op, a))
        (v, op, a) match {
          case (V2(), Dot(), V2()) => scalarizeV2DotV2(v, a)
          case (V2(), Cross(), V2()) => scalarizeV2CrossV2(v, a)
          case _ => typed(Apply(Select(v, op), List(a)))
        } 
      //case Select(v, op) => typed(Select(typed(transform(v)), op))
      //case Select(v @ V2(), _) => throw BackOut("Not Yet implemented: " + tree)
      case Ident(name) if isV2(tree.tpe) =>
        println("SCALAR V2:" + tree)
        tree

      case F() =>
        println("calling super for: " + tree)
        super.transform(tree)
      case Scalar() => tree match {
        case Literal(c @ Constant(_)) => super.transform(Literal(c.floatValue))
        case _ => typed(Select(super.transform(tree), ToFloat))
      }
      case _ => tree//throw BackOut(tree.toString)
    }

    def scalarizeLengthV2(v: Tree) = v match {
      case Ident(name) if inlinedVs.contains(name) =>
        val iv = inlinedVs(name)
        iv.forceLengthCaching
        Ident(iv.lengthN.get)
      /*case t if t.symbol != null && t.symbol.isStable =>
        val iv = ANONYMOUS*/ 
      case _=>
        // worst case, we create a temp vector
        Select(deScalar(v), Length)
    }

    def scalarizeLengthSqrV2(v: Tree) = typed(Apply(
      Select(Apply(Select(scalar(v, X), nme.MUL), List(scalar(v, X))),
      nme.ADD),
      List(Apply(Select(scalar(v, Y), nme.MUL), List(scalar(v, Y))))
    ))

    def scalarizeV2DotV2(a: Tree, v: Tree) = {
      val x = Apply(Select(scalar(a, X), nme.MUL), List(scalar(v, X)))
      val y = Apply(Select(scalar(a, Y), nme.MUL), List(scalar(v, Y)))
      typed(Apply(Select(x, nme.ADD), List(y)))
    }
    def scalarizeV2CrossV2(a: Tree, v: Tree) = {
      val x = Apply(Select(scalar(a, X), nme.MUL), List(scalar(v, Y)))
      val y = Apply(Select(scalar(a, Y), nme.MUL), List(scalar(v, X)))
      typed(Apply(Select(x, nme.SUB), List(y)))
    }
  }

}
