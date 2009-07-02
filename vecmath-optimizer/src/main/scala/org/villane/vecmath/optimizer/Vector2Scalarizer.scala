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

  /**
   * Turns a Vector2 expression into a scalar expression
   */
  class V2Scalarizer(unit: CompilationUnit, coord: Name) extends TypingTransformer(unit) {
    def typed(tr: Tree) = localTyper.typed(tr)
    def typed(tr: Tree, mode: Int, tp: Type) = localTyper.typed(tr, mode, tp)

    override def transform(tree: Tree): Tree = tree match {
      // V2 consts
      case Select(v, Zero) =>
        println(v + " -> Zero")
        println(v.tpe)
        println(V2O.tpe)
        println(v.tpe =:= V2O.tpe)
        typed(Literal(0f))
      case Select(v, One) => Literal(1f)
      case Select(v, XUnit) => Literal(if (coord == X) 1f else 0f)
      case Select(v, YUnit) => Literal(if (coord == Y) 1f else 0f)
      // must do this before transforming children
      case Select(v, op @ Normalize()) => scalarizeNormalizeV2(v, op, coord)
      case Select(v, Length) => scalarizeLengthV2(v)
      case Select(v, LengthSqr) => scalarizeLengthSqrV2(v)

      case Select(v0, op) =>
        val v = try {
          transform(v0)
        } catch {
          case e: BackOut => v0// don't actually back out!
        }
        import nme.UNARY_-
        (v, op) match {
          case (V2(), UNARY_-) => scalarizeSimpleUnaryV2(v, op, coord)
          case (V2(), Abs) => scalarizeAbsV2(v, coord)
          //case (V2(), Normalize()) => scalarizeNormalizeV2(v, op, coord)
          case _ => typed(Select(v, op)) match {
            case vTrans @ V2() => typed(scalar(vTrans, coord))
            case x => x// ? backout
          }
        }

      case Apply(fun, args) => (fun, args) match {
        // Remove float extensions
        case (Select(v, FloatExt), List(f)) if (v.tpe =:= VecMath.tpe) =>
          transform(f)

        case (Select(v0, op), List(a0)) =>
          val v = transform(v0)
          val a = transform(a0)
          (v, op, a) match {
            case (V2(), SimpleV2F(), F()) =>
              scalarizeSimpleV2FV2(v, a, op, coord)
            case (V2(), Cross(), F()) =>
              scalarizeV2CrossF(v, a, coord)
            case (V2(), SimpleV2V2(), V2()) =>
              scalarizeSimpleV2V2V2(v, a, op, coord)
            case (F(), SimpleV2F(), V2()) =>
              scalarizeSimpleFV2V2(v, a, op, coord)
            case (F(), Cross(), V2()) =>
              scalarizeFCrossV2(v, a, coord)
            case (F(), _, F()) =>
              typed(Apply(Select(v, op), List(a)))
            case _ =>
              throw BackOut(tree.toString)
          }
      }

      case V2() => super.transform(tree)
      case F() => super.transform(tree)
      case _ => throw BackOut(tree.toString)
    }

    def scalarizeSimpleUnaryV2(v: Tree, op: Name, coord: Name) =
      typed(Apply(Select(scalar(v, coord), op), Nil))
    def scalarizeAbsV2(v: Tree, coord: Name) =
      typed(Apply(Select(StdMath, Abs), List(scalar(v, coord))))
    def scalarizeNormalizeV2(v: Tree, op: Name, coord: Name) = v match {
      case Ident(name) if inlinedVs.contains(name) =>
        val iv = inlinedVs(name)
        iv.forceLengthCaching
        scalarizeSimpleV2FV2(v, Ident(iv.lengthN.get), nme.DIV, coord)
      /*case t if t.symbol != null && t.symbol.isStable =>
        val iv = ANONYMOUS*/ 
      case _=>
        // worst case, we create a temp vector
        Select(Select(deScalar(v), op), coord)
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

    def scalarizeSimpleFV2V2(a: Tree, v: Tree, op: Name, coord: Name) =
      typed(Apply(Select(a, op), List(scalar(v, coord))))
    def scalarizeSimpleV2FV2(v: Tree, a: Tree, op: Name, coord: Name) =
      typed(Apply(Select(scalar(v, coord), op), List(a)))
    def scalarizeSimpleV2V2V2(v: Tree, a: Tree, op: Name, coord: Name) =
      typed(Apply(Select(scalar(v, coord), op), List(Select(a, coord))))
    def scalarizeFCrossV2(a: Tree, v: Tree, coord: Name) = coord match {
      case X => typed(Apply(Select(scalar(v, Y), nme.MUL), List(Select(a, nme.UNARY_-))))
      case Y => typed(Apply(Select(scalar(v, X), nme.MUL), List(a)))
    }
    def scalarizeV2CrossF(v: Tree, a: Tree, coord: Name) = coord match {
      case X => typed(Apply(Select(scalar(v, Y), nme.MUL), List(a)))
      case Y => typed(Apply(Select(scalar(v, X), nme.MUL), List(Select(a, nme.UNARY_-))))
    }
    
  }
}
