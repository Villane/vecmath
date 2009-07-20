package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait Vector2Scalarizer extends ScalarizerSupport with ScalarReplacementInfo { self: VecMathOptimizer =>
  import global._
  import definitions._

  val V2T = definitions.getClass("org.villane.vecmath.Vector2").tpe
  val V2O = definitions.getModule("org.villane.vecmath.Vector2")

  def isV2(tpe: Type) = tpe == V2T || tpe.widen == V2T
  object V2 {
    def apply() = TypeTree(V2T)
    def unapply(tpe: Type): Boolean = isV2(tpe)
    def unapply(tr: Tree): Boolean = unapply(tr.tpe)
  }
  object V2Object {
    def apply() = V2O
    def unapply(tpe: Type): Boolean = tpe == V2O.tpe || tpe.widen == V2O.tpe
    def unapply(tr: Tree): Boolean = unapply(tr.tpe)
  }

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

    def expectV2X[T](block: => T) = expecting(Scalarized(V2T, X))(block)
    def expectV2Y[T](block: => T) = expecting(Scalarized(V2T, Y))(block)

    object V2Scalarizable extends Scalarizable {
      type ClassType = Vector2
      val compObject = Some(V2O)
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

      def componentFromArgs(args: List[Tree], comp: Name) = comp match {
        case X => args(0)
        case Y => args(1)
      }

      def scalarValue(v: Vector2, name: Name) = name match {
        case X => v.x
        case Y => v.y
      }

      def newNormalVar(vDef: ValDef) = new IV2(vDef.name, vDef) with NormalVariable
      def newScalarizedVar(vDef: ValDef) = new IV2(vDef.name, vDef) with ScalarizedVariable
      def newScalarizedVar(name: Name, tree: Tree) = new IV2(name, tree) with ScalarizedVariable

      def optimizeSelect(tree: Select) = optimizeV2UnaryOp(tree)
    }

    /** inlined vector2 */
    abstract class IV2(override val name: Name, override val vDef: Tree) extends Variable(name, vDef) {
      val sc = V2Scalarizable

      def x = components(X)
      def y = components(Y)

      var lengthN: Option[TermSymbol] = None
      var lengthDirty = false

      def forceLengthCaching =
        if (lengthN.isEmpty) {
          createLengthVal
        } else if (lengthDirty) {
          lengthDirty = false
          createLengthAssign
        } else {
          EmptyTree
        }

      def createLengthAssign = {
        assert(lengthN.isDefined && lengthN.get.isVariable)
        val ln = lengthN.get
        val lenSqr = UTBinOp(
          UTBinOp(x, nme.MUL, x),
          nme.ADD,
          UTBinOp(y, nme.MUL, y)
        )
        val vLen = Assign(Ident(ln), typer.typed(UTBinOp(VecMath, Sqrt, lenSqr)))
        scope.preTemps += vLen
        vLen
      }

      //var lengthSqrN: Option[TermSymbol] = None
      def createLengthVal = {
        //val lnSqr = vDef.symbol.newValue(vDef.pos, vDef.name + "$lengthSquared")
        //lengthSqrN = Some(lnSqr)
        //lnSqr.setInfo(FT)
        val ln = scope.method.symbol.newValue(scope.method.pos, name + "$length")
        lengthN = Some(ln)
        ln.setInfo(FT).setFlag(SYNTHETIC)
        if (isMutable) {
          //lnSqr.setFlag(MUTABLE)
          ln.setFlag(MUTABLE)
        } else {
          //lnSqr.setFlag(FINAL)
          ln.setFlag(FINAL)
        }
        val lenSqr = UTBinOp(
          UTBinOp(x, nme.MUL, x),
          nme.ADD,
          UTBinOp(y, nme.MUL, y)
        )
        val vLen = ValDef(ln, typer.typed(UTBinOp(VecMath, Sqrt, lenSqr)))
        scope.preTemps += vLen
        vLen
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

    def optimizeV2UnaryOp(tree: Select) = {
      val v = tree.qualifier
      val op = tree.selector
      op match {
        case Length => scalarizeV2Length(v)
        case LengthSqr => scalarizeV2LengthSquared(v)
        case Theta() => scalarizeV2Theta(v)
        case Normal => expecting match {
          case Scalarized(_, X) => expectV2Y(xf(v))
          case Scalarized(_, Y) => UnOp(expectV2X(xf(v)), nme.UNARY_-)
          case _ => superXf(tree)
        }
        case Swap => expecting match {
          case Scalarized(_, X) => expectV2Y(xf(v))
          case Scalarized(_, Y) => expectV2X(xf(v))
          case _ => superXf(tree)
        }
        case nme.UNARY_- => UnOp(xf(v), nme.UNARY_-)
        case Abs => expecting match {
          case Scalarized(_, comp) => BinOp(StdMath, Abs, transform(v))
          case _ => superXf(tree)
        }
        case Normalize() => expecting match {
          case Scalarized(_, comp) =>
            BinOp(xf(v), nme.DIV, scalarizeV2Length(v))
          case ActualType(_) => NewObj(V2(), expectV2X(xf(tree)), expectV2Y(xf(tree)))
          case _ => superXf(tree)
        }
        case _ => superXf(tree)
      }
    }

    
    // Scalar returning UnOps

    def scalarizeV2Length(v: Tree) = v match {
      case Ident(name) if scope.inlinedVar(name).isDefined || scope.normalVar(name).isDefined =>
        val iv = scope(name).asInstanceOf[IV2]
        val ref = expecting match {
          case ActualType(_) => iv.lengthN.isDefined && !iv.lengthDirty
          case _ => true
        }
        if (ref) {
          iv.forceLengthCaching
          typed(Ident(iv.lengthN.get))
        } else
          MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))
      case _ if !shouldScalarize(v, V2Scalarizable) =>
        // If it's a reference, not an expression we can optimize, cache only the length!
        val cvn = scope.cache.get(Select(v, Length)) match {
          case Some(ScalarVar(sym, _, _)) => sym
          case None => cacheS(Select(v, Length), MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v)), "length")
        }
        Ident(cvn)
      case _ =>
        // Otherwise create a new anonymous vector
        val cvn = scope.cache.get(v) match {
          case Some(ScalarVar(sym,_,_)) => sym
          case None => cacheV2(v)
        }
        val iv = scope(cvn.name).asInstanceOf[IV2]
        iv.forceLengthCaching
        typed(Ident(iv.lengthN.get))
      /*case _ =>
        MathFun(VecMath, Sqrt, scalarizeV2LengthSquared(v))*/
        // worst case, we create a temp vector
        // UnOp(deScalar(v), Length)
    }

    def scalarizeV2LengthSquared(v: Tree) = {
      val x = BinOp(expectV2X(xf(v)), nme.MUL, expectV2X(xf(v)))
      val y = BinOp(expectV2Y(xf(v)), nme.MUL, expectV2Y(xf(v)))              
      BinOp(x, nme.ADD, y)
    }

    def scalarizeV2Theta(v: Tree) =
      MathFun(VecMath, ATan2, expectV2Y(xf(v)), expectV2X(xf(v)))

    // Scalar returning BinOps

    def scalarizeV2DotV2(l: Tree, r: Tree) = {
      val x = BinOp(expectV2X(xf(l)), nme.MUL, expectV2X(xf(r)))
      val y = BinOp(expectV2Y(xf(l)), nme.MUL, expectV2Y(xf(r)))
      BinOp(x, nme.ADD, y)
    }

    def scalarizeV2CrossV2(l: Tree, r: Tree) = {
      val x = BinOp(expectV2X(xf(l)), nme.MUL, expectV2Y(xf(r)))
      val y = BinOp(expectV2Y(xf(l)), nme.MUL, expectV2X(xf(r)))
      BinOp(x, nme.SUB, y)
    }

  }

}
