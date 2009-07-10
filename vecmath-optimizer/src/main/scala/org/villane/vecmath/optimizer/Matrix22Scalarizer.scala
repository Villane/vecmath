package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait Matrix22Scalarizer extends ScalarizerSupport with ScalarReplacementInfo { self: VecMathOptimizer =>
  import global._
  import definitions._

  val M22T = definitions.getClass("org.villane.vecmath.Matrix22").tpe
  val M22O = definitions.getModule("org.villane.vecmath.Matrix22")

  def isM22(tpe: Type) = tpe == M22T || tpe.widen == M22T
  object M22 {
    def apply() = TypeTree(M22T)
    def unapply(tpe: Type): Boolean = isM22(tpe)
    def unapply(tr: Tree): Boolean = unapply(tr.tpe)
  }
  object M22Object {
    def apply() = M22O
    def unapply(tpe: Type): Boolean = tpe == M22O.tpe || tpe.widen == M22O.tpe
    def unapply(tr: Tree): Boolean = unapply(tr.tpe)
  }

  // Scalar components
  val A11 = newTermName("a11")
  val A12 = newTermName("a12")
  val A21 = newTermName("a21")
  val A22 = newTermName("a22")
  // Vector components (computed)
  val Col1 = newTermName("col1")
  val Col2 = newTermName("col2")

  // Constant names
  val Identity = newTermName("Identity")

  // Creator names
  val Rotation = newTermName("rotation")

  trait M22Transformer { self: VMTransformer =>

    object M22Scalarizable extends Scalarizable {
      type ClassType = Matrix22
      val compObject = Some(M22O)
      val classType = M22T

      val scalarComponents = List(A11, A12, A21, A22)

      val constants = Map(
        Zero -> Matrix22.Zero,
        One -> Matrix22.One,
        Identity -> Matrix22.Identity
      )

      def creators = Map(
        // TODO how to support apply(vec2,vec2) -> scalarizeM22Apply,
        Rotation -> scalarizeM22Rotation
      )

      def scalarValue(m: Matrix22, name: Name) = name match {
        case A11 => m.a11
        case A12 => m.a12
        case A21 => m.a21
        case A22 => m.a22
      }

      def newNormalVar(vDef: ValDef) = new IM22(vDef.name, vDef) with NormalVariable
      def newScalarizedVar(vDef: ValDef) = new IM22(vDef.name, vDef) with ScalarizedVariable

      def optimizeSelect(m: Select) = m

    }

    abstract class IM22(override val name: Name, override val vDef: Tree) extends Variable(name, vDef) {
      val sc = M22Scalarizable
      def a11 = components(A11)
      def a12 = components(A12)
      def a21 = components(A21)
      def a22 = components(A22)
    }

    def scalarizeM22Rotation(args: Seq[Tree], comp: Name) = {
      val angle = xf(args(0))
      // TODO MUST CACHE cos & sin in variables!!!
      // (c, -s, s, c)
      comp match {
        case A11 => MathFun(VecMath, Cos, angle)
        case A12 => UnOp(MathFun(VecMath, Sin, angle), nme.UNARY_-)
        case A21 => MathFun(VecMath, Sin, angle)
        case A22 => MathFun(VecMath, Cos, angle)
      }
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

    def scalarizeT2MulV2(t: Tree, v: Tree) = expecting match {
      // = pos + (rot * v)
      case Scalarized(V2T, _) => xf(BinOp(
        Select(t, Pos),
        nme.ADD,
        BinOp(Select(t, Rot), nme.MUL, v)
      ))
      // X = pos.x + rot.a11 * v.x + rot.a12 * v.y 
      /*case Scalarized(V2T, X) => BinOp(
        BinOp(
          xf(Select(t, Pos)),
          nme.ADD,
          BinOp(expectA11(xf(Select(t, Rot))), nme.MUL, xf(v))
        ),
        nme.ADD,
        BinOp(expectA12(xf(Select(t, Rot))), nme.MUL, expectY(xf(v)))
      )
      // Y = pos.y + rot.a21 * v.x + rot.a22 * v.y
      case Scalarized(V2T, Y) => BinOp(
        BinOp(
          xf(Select(t, Pos)),
          nme.ADD,
          BinOp(expectA21(xf(Select(t, Rot))), nme.MUL, expectX(xf(v)))
        ),
        nme.ADD,
        BinOp(expectA22(xf(Select(t, Rot))), nme.MUL, xf(v))
      )*/
    }

    def scalarizeT2MulTransV2(t: Tree, v: Tree) = expecting match {
      // = rot ** (v - pos)
      case Scalarized(V2T, _) => xf(BinOp(
        Select(t, Rot),
        MulTrans,
        BinOp(v, nme.SUB, Select(t, Pos))
      ))
    }

  }

}
