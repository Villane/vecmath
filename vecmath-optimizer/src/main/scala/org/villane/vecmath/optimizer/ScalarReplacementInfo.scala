package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait ScalarReplacementInfo { self: VecMathOptimizer =>
  import global._
  import definitions._

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
  def inScope[T](tree: Tree)(block: => T) = {
    enterScope(ScopeInfo(currentScope, tree))
    try { block } finally exitScope
  }

  case class ScopeInfo(parent: Option[ScopeInfo], tree: Tree) {

    def method = {
      var scp = this
      while (scp.parent.isDefined) scp = scp.parent.get
      if (scp.tree.isInstanceOf[DefDef])
        scp.tree
      else
        throw new Error("Not fun!")
    }

    /*def v2(name: Name) = inlinedVar(name).get.asInstanceOf[IV2]
    def hasV2(name: Name) = inlinedVar(name) match {
      case Some(IV2(_,_)) => true
      case _ => false
    }*/
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
      method.symbol.newValue(method.pos, "$anon$" + namePart + n)
    }

    // other generated variables
    val genVars = new collection.mutable.HashMap[Name, Symbol]

    val cache = new collection.mutable.HashMap[Tree, Symbol] {
      override def elemEquals(key1: Tree, key2: Tree) = key1 equalsStructure key2
      override def elemHashCode(key: Tree) = key.hashCodeStructure
    }

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
  abstract class Inlined(val name: Name, val vDef: Tree) {
    def isMutable = vDef match {
      case vDef @ ValDef(_,_,_,_) => vDef.mods.isVariable
      case _ => false
    }
    def components: Map[Name, TermSymbol]
    def scalar(comp: Name): TermSymbol
    def deScalarize: Tree
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

  class GenericScalarized(val sc: Scalarizable, override val name: Name, override val vDef: Tree)
    extends Inlined(name, vDef) {

    val components = Map(sc.scalarComponents map { n =>
      // used to be: if (vDef.isInstanceOf[ValDef]) vDef.symbol.newValue(vDef.pos, name + "$x")
      val cSym = scope.method.symbol.newValue(vDef.pos, name + "$" + n)
      cSym.setInfo(NativeScalar).setFlag(SYNTHETIC)
      cSym.setFlag(if (isMutable) MUTABLE else FINAL)
      n -> cSym
    }:_*)

    val componentUsageCount = collection.mutable.Map[Name, Int](sc.scalarComponents map {
      n => n -> 0
    }:_*)

    def scalar(comp: Name) = {
      usageCount += 1
      componentUsageCount(comp) += 1
      components(comp)
    }

    def deScalarize = {
      deScalarCount += 1
      sc.deScalarize(components mapElements { n => Ident(n) })
    }

  }

  // Scalarize for specific coordinate (X,Y,A11,A12 etc.)
  def scalar(v: Tree, coord: Name) = v match {
    case Ident(name) if scope.inlinedVar(name).isDefined =>
      Ident(scope(name).scalar(coord))
    case _ => Select(v, coord)
  }

  // Descaralrize an escaping variable. Ex. new Vector2(v$x, v$y)
  def deScalarize(v: Tree) = v match {
    case Ident(name) if scope.inlinedVar(name).isDefined =>
      scope(name).deScalarize
    case _ => v
  }

}
