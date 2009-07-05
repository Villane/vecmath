package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait ScalarReplacementInfo { self: VecMathTransformer =>
  import global._
  import definitions._             // standard classes and methods

  def shouldOptimize(m: MemberDef) = m.mods.annotations.exists(_.tpe == srAnnot.tpe)

  trait ExpectedType {
    def tpe: Type
  }
  case object NoExpectations extends ExpectedType {
    val tpe = NoType
  }
  case class Escaping(name: Name, tpe: Type) extends ExpectedType
  case class ActualType(tpe: Type) extends ExpectedType
  case class Scalarized(objType: Type, comp: Name) extends ExpectedType {
    val tpe = NativeScalar
  }

  //var stack = new collection.immutable.Stack[ScopedInfo]

  //def isOutermost = stack.size == 1
  def isOutermost = currentScope.isDefined && scope.parent.isEmpty

  var currentScope: Option[ScopeInfo] = None
  // Use this when you now a scope exists!
  def scope = currentScope.get

  def enterScope(scope: ScopeInfo) = currentScope = Some(scope)
  def exitScope = {
    if (currentScope.isDefined) currentScope = scope.parent
    if (currentScope.isEmpty) {
      // we exit the outermost scope
      // TODO flatten!
    }
  }

  case class ScopeInfo(parent: Option[ScopeInfo], tree: Tree) {
    def v2(name: Name) = inlinedVar(name).get.asInstanceOf[IV2]
    def hasV2(name: Name) = inlinedVar(name) match {
      case Some(IV2(_)) => true
      case _ => false
    }
    def apply(name: Name) = inlinedVar(name).get
    def update(name: Name, iv: Inlined) = scope.inlinedVs(name) = iv

    // statements inserted !before! the current scalarization being done
    val preTemps = new collection.mutable.ListBuffer[Tree]
    //val returnType = tree.tpe
    var _genV2 = Stream.from(0)
    def nextV2 = next("v")
    def next(namePart: String) = {
      val n = _genV2.head
      _genV2 = _genV2.tail
      tree.symbol.newValue(tree.pos, "$anon$" + namePart + n)
    }
    val cache = collection.mutable.Map[Tree, Name]()
    def cached(v: Tree) = {
      var c = cache
      while (! (c contains v) && parent.isDefined) c = parent.get.cache
      c.get(v)
    }

    def isMehtod = tree.isInstanceOf[DefDef]
    def inlinedVar(name: Name) = {
      var vs = inlinedVs
      while (! (vs contains name) && parent.isDefined) vs = parent.get.inlinedVs
      vs.get(name)
    }
    // inlined Vectors per method
    val inlinedVs = collection.mutable.Map[Name, Inlined]()
    val backoutVs = collection.mutable.Map[Name, Inlined]()
  }

  case class BackOut(msg: String) extends Throwable(msg) {
    var iv: Option[Inlined] = None
  }

  /* inlined variable, actually scalar replaced variable */
  sealed abstract class Inlined(val vDef: ValDef) {
    def isMutable = vDef.mods.isVariable
    def scalar(comp: Name): TermSymbol
    def deScalar: Tree
    var backedOut = false
    // count usage as scalars
    var usageCount = 0
    // count descalarization
    var deScalarCount = 0
  }

  /*def anonIV2(tree: Tree) = {
    val sym = nextV2
    sym.clearFlag(MUTABLE)
    val tr = ValDef(sym, EmptyTree)
    val iv = IV2(tr)
    inlinedVs(name) = iv
    preTemps += typed(ValDef(iv.x, vectorToScalar(tree, X)))
    preTemps += typed(ValDef(iv.y, vectorToScalar(tree, Y)))
  }*/

  /** inlined vector2 */
  case class IV2(override val vDef: ValDef) extends Inlined(vDef) {
    def scalar(comp: Name) = {
      usageCount += 1
      comp match {
        case X =>
          xUsageCount += 1
          x
        case Y =>
          yUsageCount += 1
          y
      }
    }
    def deScalar = {
      deScalarCount += 1
      New(V2(), List(List(Ident(x), Ident(y))))
    }

    val x = vDef.symbol.newValue(vDef.pos, vDef.name + "$x")
    val y = vDef.symbol.newValue(vDef.pos, vDef.name + "$y")
    var xUsageCount = 0
    var yUsageCount = 0

    x.setInfo(FT)
    y.setInfo(FT)
    if (vDef.mods.isVariable) {
      x.setFlag(MUTABLE)
      y.setFlag(MUTABLE)
    }

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
        UTBinOp(Ident(x), nme.MUL, Ident(x)),
        nme.ADD,
        UTBinOp(Ident(y), nme.MUL, Ident(y))
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
      val ln = vDef.symbol.newValue(vDef.pos, vDef.name + "$length")
      lengthN = Some(ln)
      ln.setInfo(FT)
      if (vDef.mods.isVariable) {
        //lnSqr.setFlag(MUTABLE)
        ln.setFlag(MUTABLE)
      }
      val lenSqr = UTBinOp(
        UTBinOp(Ident(x), nme.MUL, Ident(x)),
        nme.ADD,
        UTBinOp(Ident(y), nme.MUL, Ident(y))
      )
      val vLen = ValDef(ln, typer.typed(UTBinOp(VecMath, Sqrt, lenSqr)))
      scope.preTemps += vLen
      vLen
    }
  }

  case class IM22(override val vDef: ValDef) extends Inlined(vDef) {
    def scalar(coord: Name) = {
      usageCount += 1
      coord match {
        case A11 => a11UsageCount += 1; a11
        case A11 => a12UsageCount += 1; a12
        case A11 => a21UsageCount += 1; a21
        case A11 => a22UsageCount += 1; a22
      }
    }
    def deScalar = {
      deScalarCount += 1
      typer.typed(New(M22(), List(List(
        Ident(a11),
        Ident(a12),
        Ident(a21),
        Ident(a22)
      ))))
    }

    val a11 = vDef.symbol.newValue(vDef.pos, vDef.name + "$a11")
    val a12 = vDef.symbol.newValue(vDef.pos, vDef.name + "$a12")
    val a21 = vDef.symbol.newValue(vDef.pos, vDef.name + "$a21")
    val a22 = vDef.symbol.newValue(vDef.pos, vDef.name + "$a22")
    var a11UsageCount = 0
    var a12UsageCount = 0
    var a21UsageCount = 0
    var a22UsageCount = 0

    a11.setInfo(FT)
    a12.setInfo(FT)
    a21.setInfo(FT)
    a22.setInfo(FT)
    if (vDef.mods.isVariable) {
      a11.setFlag(MUTABLE)
      a12.setFlag(MUTABLE)
      a21.setFlag(MUTABLE)
      a22.setFlag(MUTABLE)
    }

  }

  // Scalarize for specific coordinate (X | Y)
  def scalar(v: Tree, coord: Name) = v match {
    case Ident(name) if scope.inlinedVar(name).isDefined =>
      Ident(scope.v2(name).scalar(coord))
    case _ => Select(v, coord)
  }

  // Descaralrize an escaping variable. Ex. new Vector2(v$x, v$y)
  def deScalar(v: Tree) = v match {
    case Ident(name) if scope.inlinedVar(name).isDefined =>
      scope(name).deScalar
    case _ => v
  }

}
