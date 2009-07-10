package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

/**
 * Decides which methods to optimize and performs transformations on these methods.
 */
class VecMathOptimizer(val global: Global) extends PluginComponent
  with Transform
  with TypingTransformers
  with VecMathTypes
  with VecMathOperations
  with ScalarReplacementInfo
  with ScalarizerSupport
  with Vector2Scalarizer
  with Matrix22Scalarizer
{
  import global._
  import definitions._             // standard classes and methods
  //import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

  val runsAfter = "refchecks"
  val phaseName = "vecmathopt"

  def newTransformer(unit: CompilationUnit) = new OptimizableMethodSelector(unit)

  class OptimizableMethodSelector(cu: CompilationUnit) extends Transformer {
    val vmTransformer = new VMTransformer(cu)

    def shouldOptimize(m: MemberDef) = m match {
      case ClassDef(_,_,_,_) => m.mods.annotations.exists(_.tpe == srAnnot.tpe)
      case ModuleDef(_,_,_) => m.mods.annotations.exists(_.tpe == srAnnot.tpe)
      case _ => !m.mods.annotations.exists(_.tpe == nosrAnnot.tpe)
    }

    override def transform(tree: Tree): Tree = tree match {
      // Skip unannotated classes and objects
      case tr @ ClassDef(_,_,_,_) if (!shouldOptimize(tr)) => tr
      case tr @ ModuleDef(_,_,_) if (!shouldOptimize(tr)) => tr

      case tr @ DefDef(mods, name, _, _, _, _) if (shouldOptimize(tr)) =>
        println("optimizing def " + name + " {")
        println("--- ORIGINAL SOURCE ---")
        println(tr)
        val res = vmTransformer.transform(tr)
        println("--- OPTIMIZED SOURCE ---")
        println(res)
        println("--- END ---")
        println("} //" + name)
        res

      case _ => super.transform(tree)
    }
  }

  def isReference(tree: Tree): Boolean = isReference(tree, V2.unapply)

  // TODO isReference should be any statement that returns a vector/matrix
  // except those that are handled by the scalarizers 
  def isReference(tree: Tree, typeCheck: Tree => Boolean): Boolean = tree match {
    // TODO only back out of scalarizing references if their length is not accessed (more than once?)
    case Select(v, _) if !typeCheck(v) => isReference(v, typeCheck)
    case Ident(_) => true
    case Apply(sel, _) if sel == Array_apply && isV2(tree.tpe) => true
    case If(cond, thenp, elsep) => isReference(thenp) || isReference(elsep)
    case _ => false
  }

  class VMTransformer(cu: CompilationUnit) extends TypingTransformer(cu)
    with TransformerSupport
    with V2Transformer
    with M22Transformer {

    def typed(tr: Tree) = try { localTyper.typed(tr) } catch { case e =>
      println("error:" + tr)
      throw e
    }

    // Alias to transform
    def xf(tree: Tree) = transform(tree)
    // Alias to super.transform
    def superXf(tree: Tree) = super.transform(tree)

  var currentVar: Option[ScalarizedVariable] = None
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

    def cacheS(cacheFor: Tree, v: Tree, namePart: String) = {
      // TODO analyse tree for immutability!
      val temp = true
      val name = scope.next(namePart)
      name.setInfo(FT)
      val vd = typed(ValDef(name, v))
      scope.preTemps += vd
      scope.cache(cacheFor) = ScalarVar(name, temp, v)
      name
    }

    def cacheV2(v: Tree) = {
      // TODO analyse tree for immutability!
      val temp = true
      val name = scope.nextV2
      // TODO is mutable?
      val iv = V2Scalarizable.newScalarizedVar(name.name, v)
      scope.inlinedVar(name.name) = iv
      val xRhs = expectX(transform(v))
      val xv = typed(ValDef(iv.x.symbol, xRhs))
      scope.cache(xRhs) = ScalarVar(iv.x.symbol, temp, xRhs)
      val yRhs = expectY(transform(v))
      val yv = typed(ValDef(iv.y.symbol, yRhs))
      scope.cache(yRhs) = ScalarVar(iv.y.symbol, temp, yRhs)
      scope.preTemps += xv
      scope.preTemps += yv
      scope.cache(v) = ScalarVar(name, temp, v)
      name
    }

  /**---------------------
   * MAIN TRANSFORM METHOD
   * NB! THIS IS ONLY CALLED WHEN the main transformer finds a method to optimize!
   * ---------------------*/
  override def transform(tree: Tree): Tree = tree match {
    case tr @ DefDef(mods, name, a1, a2, a3, rhs) => inScope(tr) {
      try {
        val res = typed(new Flattener(cu).transform(transform(rhs)))
        typed(copy.DefDef(tr, mods, name, a1, a2, a3, res))
      } catch {
        // TODO backout must be method level
        case bo @ BackOut(msg) =>
          println("--- END (BACKED OUT) ---")
          print("Backed out (" + name + ")")
          if (bo.iv.isDefined) {
            if (bo.iv.get.isMutable) print(" var") else print(" val")
            print(" (" + bo.iv.get.name + ")")
          }
          println(": " + msg)
          println("} //" + name)
          bo.printStackTrace
          tr
      }
    }

    // Local Variable
    case tr @ ValDef(mods, name, tpe, rhs) =>
      tpe match {
        case Scalarizable(sc) if !isReference(rhs) =>
          val iv = sc.newScalarizedVar(tr)
          currentVar = Some(iv)
          scope.inlinedVar(name) = iv // TODO remove when backed out
          try {
            var valDefs = new collection.mutable.ListBuffer[Tree]
            for ((cName, cSym) <- iv.components)
              valDefs += expecting(Scalarized(sc.classType, cName)) {
                typed(ValDef(cSym.symbol, transform(rhs)))
              }
            Sequence(addPreTemps(valDefs.toList))
          } catch {
            case bo: BackOut =>
              iv.backedOut = true
              bo.iv = Some(iv)
              throw bo
          } finally {
            currentVar = None
          }
        case Scalarizable(sc) if isReference(rhs) =>
          scope.normalVar(name) = sc.newNormalVar(tr)
          super.transform(tree)
        case F() => // Float
          val res = expectS { transform(rhs) }
          val r = typed(copy.ValDef(tr, mods, name, F(), res))
          Sequence(addPreTemps(List(r)))
        case _ => // Let super handle other variables
          super.transform(tree)
      }

    // Assignment to a scalarized variable
    case Assign(Ident(name), rhs) if scope.inlinedVar(name).isDefined =>
      val iv = scope(name).asInstanceOf[ScalarizedVariable]
      var varAssigns = new collection.mutable.ListBuffer[Tree]
      try {
        for ((cName, cSym) <- iv.components)
          varAssigns += expecting(Scalarized(iv.sc.classType, cName)) {
            typed(Assign(cSym, transform(rhs)))
          }
        if (iv.isInstanceOf[IV2]) {
          val v = iv.asInstanceOf[IV2]
          if (v.isMutable && v.lengthN.isDefined) v.lengthDirty = true
        }
        Sequence(addPreTemps(varAssigns.toList))
      } catch {
        case bo: BackOut =>
          iv.backedOut = true
          bo.iv = Some(iv)
          throw bo
      }

    // Assignment to a non-scalarized variable
    case Assign(id@Ident(name), rhs) if scope.normalVar(name).isDefined =>
      val iv = scope(name).asInstanceOf[NormalVariable]
      val varAssign = expecting(Escaping(null, iv.sc.classType)) {
        typed(Assign(id, transform(rhs)))
      }
      if (iv.isInstanceOf[IV2]) {
        val v = iv.asInstanceOf[IV2]
        if (v.isMutable && v.lengthN.isDefined) v.lengthDirty = true
      }
      Sequence(addPreTemps(List(varAssign)))

    // New Vector2
    case tr @ Apply(Select(New(V2()), nme.CONSTRUCTOR), List(x, y)) => expecting match {
      case Scalarized(V2T, X) => expectS(xf(x))
      case Scalarized(V2T, Y) => expectS(xf(y))
      case Escaping(_, V2T) => NewObj(V2(), expectX(xf(x)), expectY(xf(y)))
      case ActualType(V2T) => NewObj(V2(), expectX(xf(x)), expectY(xf(y)))
      case _ => super.transform(tree)
    }

    // HACK this is a workaround for a bug in Scala 2.7.5, SCreator will not match!!!
    case Apply(Select(ScalarizableObject(sc), name), args) if sc.creators contains name =>
    // Scalarizable creators e.g. Vector2.polar(...)
    // case SCreator(scrz, name, args) =>*/
      expecting match {
        case Scalarized(typ, comp) if sc.isClassOf(typ) => scalarizeCreator(sc, name, args, comp)
        case _ => super.transform(tree)
      }

    // Scalarizable constants e.g. Vector2.XUnit, Matrix22.Identity
    case SConstant(scalarizable, name) => expecting match {
      case Scalarized(typ, comp) if scalarizable.isClassOf(typ) =>
        scalarizeConstant(scalarizable, name, comp)
      case _ => super.transform(tree)
    }

    // Scalar components (v.x, m.a11 etc.)
    case SComponent(v @ Ident(name), comp) =>
      // TODO maybe this could always be just: typed(scalar(v, op))
      if (scope.inlinedVar(name).isDefined) expecting match {
        // Apply replacements v.x -> v$x and so on
        case Scalarized(_,_) => typed(scalar(v, comp))
        case ActualType(NativeScalar) => typed(scalar(v, comp))
        case NoExpectations => typed(scalar(v, comp))
        case _ => tree
      } else tree

    // Unary operators
    case tr @ Select(Scalarizable(sc), op) => sc.optimizeSelect(tr)

    // Binary operators
    case tr @ Apply(Select(left00, op), List(arg00)) =>
      val left0 = removeFloatExt(left00)
      val arg0 = removeFloatExt(arg00)
      // Match on return type
      tr match {
        case F() =>
          (left0, op, arg0) match {
            case (V2(), Dot(), V2()) => scalarizeV2DotV2(left0, arg0)
            case (V2(), Cross(), V2()) => scalarizeV2CrossV2(left0, arg0)
            case _ => expectS(typed(Apply(Select(xf(left0), op), List(xf(arg0)))))
          }
        case V2() =>
          expecting match {
            case Scalarized(V2T, comp) =>
              (left0, op, arg0) match {
                // V2 op Float or vice versa
                case (V2(), SimpleV2F(), F()) =>
                  BinOp(xf(left0), op, expectS(xf(arg0)))
                case (V2(), Cross(), F()) => comp match {
                  case X => BinOp(expectY(xf(left0)), nme.MUL, expectS(xf(arg0)))
                  case Y => BinOp(expectX(xf(left0)), nme.MUL, UnOp(expectS(xf(arg0)), nme.UNARY_-))
                }
                case (F(), SimpleV2F(), V2()) =>
                  BinOp(expectS(xf(left0)), op, xf(arg0))
                case (F(), Cross(), V2()) => comp match {
                  case X => BinOp(expectY(xf(arg0)), nme.MUL, UnOp(expectS(xf(left0)), nme.UNARY_-))
                  case Y => BinOp(expectX(xf(arg0)), nme.MUL, expectS(xf(left0)))
                }
                // V2 op V2
                case (V2(), SimpleV2V2(), V2()) =>
                  BinOp(xf(left0), op, xf(arg0))
                // M2 op V2
                case (M22(), nme.MUL, V2()) => scalarizeM22MulV2(left0, arg0)
                case (M22(), MulTrans, V2()) => scalarizeM22MulTransV2(left0, arg0)
                // T2 op V2
                case (T2(), nme.MUL, V2()) => scalarizeT2MulV2(left0, arg0)
                case (T2(), MulTrans, V2()) => scalarizeT2MulTransV2(left0, arg0)

                // Other
                case _ =>
                  // for example array.apply(idx) -> array.apply(idx).x
                  typed(scalar(BinOp(xf(left0), op, xf(arg0)), comp))
              }
            case Escaping(_,_) => //BinOp(xf(left0), op, xf(arg0))
              NewObj(V2(), expectX(xf(tr)), expectY(xf(tr)))
            case _ => BinOp(xf(left0), op, xf(arg0))
          }
        case _ => super.transform(tr)
      }

    // Reference to scalarized variable
    case Ident(name) if scope.inlinedVar(name).isDefined => expecting match {
      // Scalarize if necessary
      case Scalarized(_, comp) => typed(scalar(tree, comp))
      // Descalarize if escaping
      case Escaping(name, _) => typed(deScalarize(tree))
      // ONLY DESCALARIZE ESCAPING!!! case ActualType(_) => typed(deScalarize(tr))
      // We expect this will be scalarized in an outer scope
      case _ => tree
    }

    // Reference to non-scalarized value
    case Ident(name) if !scope.inlinedVar(name).isDefined => expecting match {
      // Scalarize if necessary
      case Scalarized(_, comp) if isScalarizable(tree) => typed(Select(tree, comp))
      // Otherwise, it's just an identifier we don't do anything with
      case _ => tree
    }

    case tr @ Block(stats, xpr @ Scalarizable(sc)) =>
      val sts = transformStats(stats, currentOwner)
      val xp = xpr match {
        case Ident(name) if scope.inlinedVar(name).isDefined =>
          // A scalarized variable is escaping!
          expecting(Escaping(name, sc.classType)) { transform(xpr) }
        //case BinOp(_,_,_) => NewObj(V2(), expectX(xf(xpr)), expectY(xf(xpr)))
        //case UnOp(_,_) => NewObj(V2(), expectX(xf(xpr)), expectY(xf(xpr)))
        case _ => expecting(Escaping(null, sc.classType)) { transform(xpr) }
      }
      // got pretemps from xpr: addPreTemps(xp)
      typed(Block(sts, xp))
    case tr @ Block(stats, xpr) =>
      val sts = transformStats(stats, currentOwner)
      val xp = xpr match {
        case _ => expecting(Escaping(null,null))(transform(xpr))
      }
      typed(Block(sts, xp))

    case tr @ Return(v@Ident(name)) if scope.inlinedVar(name).isDefined =>
      expecting(Escaping(name, tr.tpe)) { typed(Return(transform(v))) }
    case tr @ Return(v) =>
      expecting(Escaping(null, null)) { copy.Return(tr, transform(v)) }

    case tr @ If(cond, thenp, elsep) => expecting match {
      //case Scalarized(_,_) => throw BackOut("If not supported!")
      case Scalarized(_, comp) if currentVar.isDefined =>
        val ifName = currentVar.get.name + "$cond"
        val ifSym = scope.genVars.get(ifName) match {
          case Some(ScalarVar(sym,_,_)) => sym
          case None =>
            val sym = scope.method.symbol.newValue(tr.pos, currentVar.get.name + "$cond")
            sym.setInfo(BooleanClass.tpe).setFlag(SYNTHETIC).setFlag(FINAL)
            val ifV = typed(ValDef(sym, expecting(ActualType(BooleanClass.tpe))(transform(cond))))
            scope.preTemps += ifV
            // TODO analyse tree for immutability
            scope.genVars(ifName) = ScalarVar(sym, true, ifV.asInstanceOf[ValDef].rhs)
            sym
        }
        typed(If(typed(Ident(ifSym)), transform(thenp), transform(elsep)))

      case _ =>
        val cnd = transform(cond)
        var thn = transform(thenp)
        if (!scope.preTemps.isEmpty)
          thn = typed(Block(addPreTemps(Nil), thn))
        var els = transform(elsep)
        if (!scope.preTemps.isEmpty)
          els = typed(Block(addPreTemps(Nil), els))
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

  def addPreTemps(stats: List[Tree]) = {
    // reuse the preTemps ListBuffer here, since we are going to clear it anyway!
    stats foreach { stat =>
      // if we created a temp variable in preTemps, use it!
      scope.preTemps += new UseCachedIfPossible(cu).transform(stat)
    }
    val statsTrans = scope.preTemps.toList
    scope.preTemps.clear
    // drop all cached variables so they won't be reused!
    scope.cache.retain((tree, v) => !v.temp)
    statsTrans
  }

  def removeFloatExt(tree: Tree) = tree match {
    case FEXT() => tree match {
      case BinOp(/* doesn't work: VecMath*/_, FloatExt, arg0) => arg0
      case _ => tree
    }
    case _ => tree
  }
  }

  class UseCachedIfPossible(cu: CompilationUnit) extends TypingTransformer(cu) {
    var inCachedValDef = false
    
    override def transform(tree: Tree): Tree = tree match {
      case ValDef(_,_,_,rhs) =>
        if (scope.cache.get(rhs).isDefined) {
          inCachedValDef = true
          val res = super.transform(tree)
          inCachedValDef = false
          res
        } else super.transform(tree)

      case tr if !inCachedValDef => scope.cache.get(tr) match {
        case Some(ScalarVar(sym,_,_)) =>
          if (scope.inlinedVar(sym.name).isEmpty)
            localTyper.typed(Ident(sym))
          else // we have a ref to a Vec/Mat variable that doesn't exist 
            super.transform(tree)
        case None =>
          super.transform(tree)
      }
        

      case _ => super.transform(tree)
    }

  }

  class Flattener(cu: CompilationUnit) extends TypingTransformer(cu) {

    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) =>
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
