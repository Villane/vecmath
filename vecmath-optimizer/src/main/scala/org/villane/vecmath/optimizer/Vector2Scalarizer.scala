package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait Vector2Scalarizer extends ScalarizerSupport { self: VecMathOptimizer =>
  import global._
  import definitions._

  // Scalar component names
  val X = newTermName("x")
  val Y = newTermName("y")

  // Constant names
  val XUnit = newTermName("XUnit")
  val YUnit = newTermName("YUnit")

  // Creator names
  val Polar = newTermName("polar")
  val Lerp = newTermName("lerp")

  trait V2Transformer { self: VMTransformer =>
  
    object V2Scalarizable extends Scalarizable {
      type ObjectType = Vector2.type
      type ClassType = Vector2
      val objectType = V2O.tpe
      val classType = V2T

      val scalarComponents = List(X, Y)

      val constants = Map(
        Zero -> Vector2.Zero,
        One -> Vector2.One,
        XUnit -> Vector2.XUnit,
        YUnit -> Vector2.YUnit
      )

      def creators = Map(
        Polar -> scalarizeV2Polar,
        Lerp -> scalarizeV2Lerp
      )

      def scalarValue(v: Vector2, name: Name) = name match {
        case X => v.x
        case Y => v.y
      }

    }

    // Creators

    def scalarizeV2Polar(args: Seq[Tree], comp: Name) = BinOp(
      expectS(xf(args(0))),
      nme.MUL,
      MathFun(VecMath, if (comp == X) Cos else Sin, expectS(args(1)))
    )

    def scalarizeV2Lerp(args: Seq[Tree], comp: Name) = {
      val begin = xf(args(0))
      val end = xf(args(1))
      val scalar = expectS(xf(args(2)))
      // X = begin.x + scalar * (end.x - begin.x)
      // Y = begin.y + scalar * (end.y - begin.y)
      BinOp(
        begin,
        nme.ADD,
        BinOp(scalar, nme.MUL, BinOp(end, nme.SUB, begin))
      )
    }

    // Scalar returning UnOps

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
      case tr if isReference(tr) && tr.symbol.isStable =>
        // TODO temporary cache for unstables!!!
        if (true) {
          val cvn = scope.cache.get(v) match {
            case Some(sym) => sym
            case None => cacheS(v, MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v)), "length")
          }
          Ident(cvn)
        } else
          MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))
      case tr if tr.symbol.isStable =>
        if (true) {
          val cvn = scope.cache.get(v) match {
            case Some(sym) => sym
            case None => cacheV2(v)
          }
          val iv = scope.v2(cvn.name)
          iv.forceLengthCaching
          typed(Ident(iv.lengthN.get))
        } else
          MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))
      case _ =>
        MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))
        // worst case, we create a temp vector
        // UnOp(deScalar(v), Length)
    }

    def scalarizeV2LengthSquared(v: Tree) = {
      val x = BinOp(expectX(xf(v)), nme.MUL, expectX(xf(v)))
      val y = BinOp(expectY(xf(v)), nme.MUL, expectY(xf(v)))              
      BinOp(x, nme.ADD, y)
    }

    def scalarizeV2Theta(v: Tree) =
      MathFun(VecMath, ATan2, expectY(xf(v)), expectX(xf(v)))

    // Scalar returning BinOps

    def scalarizeV2DotV2(l: Tree, r: Tree) = {
      val x = BinOp(expectX(xf(l)), nme.MUL, expectX(xf(r)))
      val y = BinOp(expectY(xf(l)), nme.MUL, expectY(xf(r)))
      BinOp(x, nme.ADD, y)
    }

    def scalarizeV2CrossV2(l: Tree, r: Tree) = {
      val x = BinOp(expectX(xf(l)), nme.MUL, expectY(xf(r)))
      val y = BinOp(expectY(xf(l)), nme.MUL, expectX(xf(r)))
      BinOp(x, nme.SUB, y)
    }

  }

}
