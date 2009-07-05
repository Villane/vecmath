package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

class VecMathTransformer(val global: Global)
  extends PluginComponent with TypingTransformers
  with VecMathTypes
  with VecMathOperations
  with ScalarReplacementInfo
  with Vector2Scalarizer
  with ScalarScalarizer
{
  val runsAfter = "refchecks"
  val phaseName = "vecmathopt"

  import global._
  import definitions._             // standard classes and methods
  //import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

  def newTransformer(unit: CompilationUnit) = new VMTransformer(unit)

  /** Create a new phase which applies transformer */
  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit) {
      newTransformer(unit).transformUnit(unit)
      new Flattener(unit).transformUnit(unit)
    }
  }

  class VMTransformer(cu: CompilationUnit) extends TypingTransformer(cu)
    with ScalarTransformer with V2Scalarizer {
    def typed(tr: Tree) = try {localTyper.typed(tr)} catch { case e =>
      println("error:" + tr)
      throw e
    }
    def typed(tr: Tree, mode: Int, tp: Type) = localTyper.typed(tr, mode, tp)

  val expectedTypes = new collection.mutable.Stack[ExpectedType]
  def expecting = if (expectedTypes.isEmpty) NoExpectations else expectedTypes.top
  def expecting[T](expType: ExpectedType)(block: => T) = {
    expectedTypes.push(expType)
    try {
      block
    } catch {
      case ex =>
        throw ex
    } finally {
      expectedTypes.pop
    }
  }

  def expect[T](tpe: Type, comp: Name)(block: => T) = expecting(Scalarized(tpe, comp))(block)
  /*def expect(tpe: Type, comp: Name)(tree: Tree) = expecting(Scalarized(tpe, comp))(transform(tree))*/

  def expectS[T](block: => T) = expecting(ActualType(NativeScalar))(block)
  def expectX[T](block: => T) = expecting(Scalarized(V2T, X))(block)
  def expectY[T](block: => T) = expecting(Scalarized(V2T, Y))(block)
  def expectA11[T](block: => T) = expecting(Scalarized(M22T, A11))(block)
  def expectA12[T](block: => T) = expecting(Scalarized(M22T, A12))(block)
  def expectA21[T](block: => T) = expecting(Scalarized(M22T, A21))(block)
  def expectA22[T](block: => T) = expecting(Scalarized(M22T, A22))(block)

  /*
  def expectS(tree: Tree) = expecting(ActualType(NativeScalar)) { transform(tree) }
  def expectX(tree: Tree) = expecting(Scalarized(V2T, X)) { transform(tree) }
  def expectY(tree: Tree) = expecting(Scalarized(V2T, Y)) { transform(tree) }
  def expectA11(tree: Tree) = expecting(Scalarized(M22T, A11)) { transform(tree) }
  def expectA12(tree: Tree) = expecting(Scalarized(M22T, A12)) { transform(tree) }
  def expectA21(tree: Tree) = expecting(Scalarized(M22T, A21)) { transform(tree) }
  def expectA22(tree: Tree) = expecting(Scalarized(M22T, A22)) { transform(tree) }
  */

    def cacheS(v: Tree, namePart: String) = {
      val name = scope.next(namePart).name
      val vd = ValDef(NoMods, name, F(), v)
      scope.preTemps += vd
      name
    }
    def cacheV2(v: Tree) = {
      val name = scope.nextV2.name
      // TODO is mutable?
      val iv = IV2(ValDef(NoMods, name, V2(), v))
      scope(name) = iv
      val xv = expectX(typed(ValDef(iv.x, transform(v))))
      val yv = expectY(typed(ValDef(iv.y, transform(v))))
      scope.preTemps += xv
      scope.preTemps += yv
      scope.cache(v) = name
      name
    }

  // Alias to transform
  def xf(tree: Tree) = transform(tree)
  /**---------------------
   * MAIN TRANSFORM METHOD
   * ---------------------*/
  override def transform(tree: Tree): Tree = tree match {

    // Skip unannotated classes and objects
    case tr @ ClassDef(_,_,_,_) if (!shouldOptimize(tr)) => tr
    case tr @ ModuleDef(_,_,_) if (!shouldOptimize(tr)) => tr

    case tr @ DefDef(mods, name, a1, a2, a3, rhs)
      if (currentScope.isEmpty && shouldOptimize(tr)) =>
      // We have a new (outer) method to be optimize!
      enterScope(ScopeInfo(None, tr))
      println("optimizing def " + name + " {")
      println("--- ORIGINAL SOURCE ---")
      println(tr)
      try {
        val res = typed(new Flattener(cu).transform(transform(rhs)))
        val r = typed(copy.DefDef(tr, mods, name, a1, a2, a3, res))
        println("--- OPTIMIZED SOURCE ---")
        println(r)
        println("--- END ---")
        println("} //" + name)
        r
      } catch {
        // TODO backout must be method level
        case bo @ BackOut(msg) =>
          println("--- END (BACKED OUT) ---")
          print("Backed out (" + name + ")")
          if (bo.iv.isDefined) {
            if (bo.iv.get.isMutable) print(" var") else print(" val")
            print(" (" + bo.iv.get.vDef.name + ")")
          }
          println(": " + msg)
          println("} //" + name)
          bo.printStackTrace
          tr
      } finally exitScope

    // Local Variable
    case tr @ ValDef(mods, name, tpe, rhs) if currentScope.isDefined =>
      tpe match {
        case V2() => // Vector2
          val iv = IV2(tr)
          try {
            scope(name) = iv
            val xv = expectX(typed(ValDef(iv.x, transform(rhs))))
            val yv = expectY(typed(ValDef(iv.y, transform(rhs))))
            val stats = scope.preTemps.toList ::: List(xv, yv)
            scope.preTemps.clear
            Sequence(stats)
          } catch {
            case bo: BackOut =>
              iv.backedOut = true
              bo.iv = Some(iv)
              throw bo
          }
        case M22() => // Matrix22
          val iv = IM22(tr)
          try {
            scope(name) = iv
            val a11v = expectA11 { typed(ValDef(iv.a11, transform(rhs))) }
            val a12v = expectA12 { typed(ValDef(iv.a12, transform(rhs))) }
            val a21v = expectA21 { typed(ValDef(iv.a21, transform(rhs))) }
            val a22v = expectA22 { typed(ValDef(iv.a22, transform(rhs))) }
            val stats = scope.preTemps.toList ::: List(a11v, a12v, a21v, a22v)
            scope.preTemps.clear
            Sequence(stats)
          } catch {
            case bo: BackOut =>
              iv.backedOut = true
              bo.iv = Some(iv)
              throw bo
          }
        case F() => // Float
          val res = expectS { transform(rhs) }
          val r = typed(copy.ValDef(tr, mods, name, F(), res))
          val stats = scope.preTemps.toList ::: List(r)
          scope.preTemps.clear
          Sequence(stats)
        case _ => // Let super handle other variables
          super.transform(tree)
      }

    // Assignment to one of the variables we inlined
    case tr @ Assign(Ident(name), rhs)
      if currentScope.isDefined && scope.inlinedVar(name).isDefined =>
      val iv = scope(name)
      try {
      iv match {
        case v @ IV2(_) =>
          val xa = expectX { typed(Assign(Ident(v.x), transform(rhs))) }
          val ya = expectY { typed(Assign(Ident(v.y), transform(rhs))) }
          val stats = scope.preTemps.toList ::: List(xa, ya)
          scope.preTemps.clear
          if (v.isMutable && v.lengthN.isDefined) v.lengthDirty = true
          Sequence(stats)
        case v @ IM22(_) =>
          val a11a = expectA11 { typed(Assign(Ident(v.a11), transform(rhs))) }
          val a12a = expectA12 { typed(Assign(Ident(v.a12), transform(rhs))) }
          val a21a = expectA21 { typed(Assign(Ident(v.a21), transform(rhs))) }
          val a22a = expectA22 { typed(Assign(Ident(v.a22), transform(rhs))) }
          val stats = scope.preTemps.toList ::: List(a11a, a12a, a21a, a22a)
          scope.preTemps.clear
          // cache determinant?
          // if (v.isMutable && v.lengthN.isDefined) v.lengthDirty = true
          Sequence(stats)
      }
      } catch {
        case bo: BackOut =>
          iv.backedOut = true
          bo.iv = Some(iv)
          throw bo
      }

    // New Vector2
    case tr @ Apply(Select(New(V2()), nme.CONSTRUCTOR), List(x, y)) if currentScope.isDefined => expecting match {
      case Scalarized(V2T, X) => expectS(xf(x))
      case Scalarized(V2T, Y) => expectS(xf(y))
      // UNTESTED:
      case ActualType(V2T) => NewObj(V2(), expectX(xf(x)), expectY(xf(y)))
      case _ => super.transform(tree)
    }

    // Binary Operator
    case tr @ Apply(Select(left00, op), List(arg00)) if currentScope.isDefined =>
      val left0 = removeFloatExt(left00)
      val arg0 = removeFloatExt(arg00)
      tr match {
        /*case FEXT() => expecting match {
          case Scalarized(V2T, comp) => (left0, op, arg0) match {
            // Remove Float extensions
            case (_, FloatExt, F()) => expectS(xf(arg0))
            case _ => throw new Error("Shouldn't get here")
          }
          case _ => super.transform(tree)
        }*/
        case F() => expectS {
          val left = transform(left0)
          val arg = transform(arg0)
          (left, op, arg) match {
            case (V2(), Dot(), V2()) => scalarizeV2DotV2(left, arg)
            case (V2(), Cross(), V2()) => scalarizeV2CrossV2(left, arg)
            case _ => typed(Apply(Select(left, op), List(arg)))
          }
        }
        case V2() =>
          expecting match {
            case Scalarized(V2T, comp) =>
              (left0, op, arg0) match {
                // V2 op Float or vice versa
                case (V2(), SimpleV2F(), F()) =>
                  BinOp(xf(left0), op, expectS(xf(arg0)))
                case (V2(), Cross(), F()) => println(left0 + " VCROSS " + arg0); comp match {
                  case X => BinOp(expectY(xf(left0)), nme.MUL, expectS(xf(arg0)))
                  case Y => BinOp(expectX(xf(left0)), nme.MUL, UnOp(expectS(xf(arg0)), nme.UNARY_-))
                }
                case (F(), SimpleV2F(), V2()) =>
                  BinOp(expectS(xf(left0)), op, xf(arg0))
                case (F(), Cross(), V2()) => println(left0 + " FCROSS " + arg0); comp match {
                  case X => BinOp(expectY(xf(arg0)), nme.MUL, UnOp(expectS(xf(left0)), nme.UNARY_-))
                  case Y => BinOp(expectX(xf(arg0)), nme.MUL, expectS(xf(left0)))
                }
                // V2 op V2
                case (V2(), SimpleV2V2(), V2()) =>
                  BinOp(xf(left0), op, xf(arg0))
                case (V2(), Dot(), V2()) =>
                  scalarizeV2DotV2(left0, arg0)
                case (V2(), Cross(), V2()) =>
                  scalarizeV2CrossV2(left0, arg0)
                // M2 op V2
                case (M22(), nme.MUL, V2()) => scalarizeM22MulV2(left0, arg0)
                case (M22(), MulTrans, V2()) => scalarizeM22MulTransV2(left0, arg0)

                // Other
                case _ =>
                  BinOp(xf(left0), op, xf(arg0))
              }
            case _ => //BinOp(xf(left0), op, xf(arg0))
              NewObj(V2(), expectX(xf(tr)), expectY(xf(tr)))
          }
        case _ => super.transform(tr)
      }

    // Vector2 consts
    case tr @ Select(V2Ob(), const) => expecting match {
      case Scalarized(V2T, comp) => const match {
        case Zero => typed(Literal(0f))
        case One => typed(Literal(1f))
        case XUnit if comp == X => typed(Literal(1f))
        case XUnit if comp == Y => typed(Literal(0f))
        case YUnit if comp == X => typed(Literal(0f))
        case YUnit if comp == Y => typed(Literal(1f))
        case _ => super.transform(tr)
      }
    }

    // Vector2 constructors
    case tr @ Apply(Select(V2Ob(), const), args) => expecting match {
      case Scalarized(V2T, comp) => const match {
        case Polar => BinOp(
          expectS(xf(args(0))),
          nme.MUL,
          MathFun(VecMath, if (comp == X) Cos else Sin, expectS(args(1)))
        )
        case _ => super.transform(tr)
      }
      case _ => super.transform(tr)
    }

    // Vector2 Unary operator
    case tr @ Select(v @ V2(), op) if currentScope.isDefined => tr match {
      case Select(_, Length) => scalarizeV2Length(v)
      case Select(_, LengthSqr) => scalarizeV2LengthSquared(v)
      case Select(_, Theta()) => scalarizeV2Theta(v)
      case Select(Ident(name), ScalarComponent())
        if scope.inlinedVar(name).isDefined => expecting match {
          // Apply replacements v.x -> v$x and so on
          case Scalarized(_,_) => typed(scalar(v, op))
          case ActualType(NativeScalar) => typed(scalar(v, op))
          case NoExpectations => typed(scalar(v, op))
          case _ => super.transform(tr)
        }
      case Select(_, Normal) => expecting match {
        case Scalarized(_, X) => expectY(xf(v))
        case Scalarized(_, Y) => UnOp(expectX(xf(v)), nme.UNARY_-)
        case _ => super.transform(tr)
      }
      case Select(_, Swap) => expecting match {
        case Scalarized(_, X) => expectY(xf(v))
        case Scalarized(_, Y) => expectX(xf(v))
        case _ => super.transform(tr)
      }
      case Select(_, nme.UNARY_-) => UnOp(xf(v), nme.UNARY_-)
      case Select(_, Abs) => expecting match {
        case Scalarized(_, comp) => BinOp(StdMath, Abs, transform(v))
        case _ => super.transform(tr)
      }
      case Select(_, Normalize()) => expecting match {
        case Scalarized(_, comp) => BinOp(xf(v), nme.DIV, scalarizeV2Length(v))
        case Escaping(_, _) => NewObj(V2(), expectX(xf(tr)), expectY(xf(tr)))
        case _ => super.transform(tr)
      }
      case _ => super.transform(tr)
    }

    case tr @ Ident(name) if currentScope.isDefined && scope.inlinedVar(name).isDefined =>
      expecting match {
        case Scalarized(_, comp) => typed(scalar(tr, comp))
        case Escaping(name, _) => typed(deScalar(tr))
        // ONLY ESCAPING!!! case ActualType(_) => typed(deScalar(tr))
        case _ => tr
      }

    case tr @ Ident(name) if currentScope.isDefined && !scope.inlinedVar(name).isDefined =>
      expecting match {
        case Scalarized(_, comp) => tree match {
          case V2() => typed(scalar(tr, comp))
          case M22() => typed(scalar(tr, comp))
          case _ => tr
        }
        //case ActualType(_) => typed(deScalar(tr))
        case _ => tr
      }

    case tr @ Block(stats, xpr @ V2()) =>
      val sts = transformStats(stats, currentOwner)
      val xp = xpr match {
        case Ident(name) if currentScope.isDefined && scope.inlinedVar(name).isDefined =>
          expecting(Escaping(name, tr.tpe)) { transform(xpr) }
        case V2() => xpr match {
          // TODO this may select things like segment.v1
          case BinOp(_,_,_) => NewObj(V2(), expectX(xf(xpr)), expectY(xf(xpr)))
          case UnOp(_,_) => NewObj(V2(), expectX(xf(xpr)), expectY(xf(xpr)))
          case _ => expecting(ActualType(V2T)) { transform(xpr) }
        }
        case _ => transform(xpr)
      }
      // got pretemps from xpr: sts ++ scope.preTemps
      typed(Block(sts, xp))
    case tr @ Block(stats, xpr) if currentScope.isDefined =>
      val sts = transformStats(stats, currentOwner)
      val xp = xpr match {
        case _ => expecting(Escaping(null,null))(transform(xpr))
      }
      typed(Block(sts, xp))

    case tr @ Return(v@Ident(name)) if currentScope.isDefined && scope.inlinedVar(name).isDefined =>
      expecting(Escaping(name, tr.tpe)) { typed(Return(transform(v))) }
    case tr @ Return(v) if currentScope.isDefined =>
      expecting(Escaping(null, null)) { copy.Return(tr, transform(v)) }

    case tr @ If(cond, thenp, elsep) if currentScope.isDefined => expecting match {
      case Scalarized(_,_) => throw BackOut("If not supported!")
      case _ =>
        val cnd = transform(cond)
        var thn = transform(thenp)
        if (!scope.preTemps.isEmpty) {
          thn = typed(Block(scope.preTemps.toList, thn))
          scope.preTemps.clear
        }
        var els = transform(elsep)
        if (!scope.preTemps.isEmpty) {
          thn = typed(Block(scope.preTemps.toList, els))
          scope.preTemps.clear
        }
        typed(If(cnd, thn, els))
    }

    // UNTESTED!!! Any other op resulting in Vector2. Somewhy This doesn't work!!
    /*case V2() => expecting match {
      case Scalarized(V2T, comp) => typed(scalar(tree, comp))
      case _ => super.transform(tree)
    }*/

    case tr =>
      if (isV2(tr.tpe)) expecting match {
        case Scalarized(V2T, comp) => typed(scalar(tree, comp))
        case _ => super.transform(tree)
      } else if (isM22(tr.tpe)) expecting match {
        case Scalarized(M22T, comp) => typed(scalar(tree, comp))
        case _ => super.transform(tree)
      } else {
        //println("Tree (" + tree + "): " + tree.tpe + ", widen: " + tree.tpe.widen)
        //println(isV2(tree.tpe))
        super.transform(tree)
      }
  }

  def removeFloatExt(tree: Tree) = tree match {
    case FEXT() => tree match {
      case BinOp(/* doesn't work: VecMath*/_, FloatExt, arg0) => arg0
      case _ => tree
    }
    case _ => tree
  }
  }

  class Flattener(cu: CompilationUnit) extends TypingTransformer(cu) {
    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) =>
        println("FLATTENING")
        val stats2 = new collection.mutable.ListBuffer[Tree]
        transformTrees(stats) foreach { stat => stat match {
          case Sequence(ls) => stats2 ++= ls
          case _ => stats2 += stat
        }}
        val expr2 = transform(expr) match {
          case Sequence(ls) => throw new Error("should not happen")
          case exp => exp
        }
        localTyper.typed(Block(stats2.toList, expr2))
      case _ => super.transform(tree)
    }
  }
}
