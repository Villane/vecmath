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

  case class ScalarVar(symbol: Symbol, temp: Boolean, tree: Tree)

  case class ScopeInfo(parent: Option[ScopeInfo], tree: Tree) {

    def isMehtod = tree.isInstanceOf[DefDef]
    def method = {
      var scp = this
      while (scp.parent.isDefined) scp = scp.parent.get
      if (scp.tree.isInstanceOf[DefDef])
        scp.tree
      else
        throw new Error("Not fun!")
    }

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
    val genVars = new collection.mutable.HashMap[Name, ScalarVar]

    val cache = new collection.mutable.HashMap[Tree, ScalarVar] {
      override def elemEquals(key1: Tree, key2: Tree) = key1 equalsStructure key2
      override def elemHashCode(key: Tree) = key.hashCodeStructure
    }

    def cached(v: Tree) = {
      var c = cache
      while (! (c contains v) && parent.isDefined) c = parent.get.cache
      c.get(v)
    }

    def apply(name: Name) = (inlinedVar(name) orElse normalVar(name)).get

    object normalVar {
      def apply(name: Name) = {
        var vs = normalVars
        while (! (vs contains name) && parent.isDefined) vs = parent.get.normalVars
        vs.get(name)
      }
      def update(name: Name, v: NormalVariable) = scope.normalVars(name) = v
    }

    object inlinedVar {
      def apply(name: Name) = {
        var vs = inlinedVars
        while (! (vs contains name) && parent.isDefined) vs = parent.get.inlinedVars
        vs.get(name)
      }
      def update(name: Name, v: ScalarizedVariable) = inlinedVars(name) = v
    }

    private val normalVars = collection.mutable.Map[Name, NormalVariable]()
    private val inlinedVars = collection.mutable.Map[Name, ScalarizedVariable]()
    private val backoutVars = collection.mutable.Map[Name, ScalarizedVariable]()
  }

  case class BackOut(msg: String) extends Throwable(msg) {
    var iv: Option[Variable] = None
  }

  /** variable, maybe scalar replaced or maybe not */
  abstract class Variable(val name: Name, val vDef: Tree) {
    def isMutable = vDef match {
      case vDef @ ValDef(_,_,_,_) => vDef.mods.isVariable
      case _ => false
    }
    def isScalarized: Boolean
    def sc: Scalarizable
    def components: Map[Name, Tree]
    def scalar(comp: Name): Tree
    def deScalarize: Tree
  }

  /** not scalar replaced variable */
  trait NormalVariable extends Variable {
    val isScalarized = false

    val components = Map(sc.scalarComponents map { n => n -> Select(Ident(vDef.symbol), n) }:_*)

    def scalar(comp: Name) = components(comp)
    def deScalarize = Ident(name)
  }

  /** scalar replaced variable */
  trait ScalarizedVariable extends Variable {
    val isScalarized = true

    // count usage of scalar components
    var usageCount = 0
    // count descalarization
    var deScalarCount = 0
    // whether scalarization was backed out of
    var backedOut = false

    val components = Map(sc.scalarComponents map { n =>
      // used to be: if (vDef.isInstanceOf[ValDef]) vDef.symbol.newValue(vDef.pos, name + "$x")
      val cSym = scope.method.symbol.newValue(vDef.pos, name + "$" + n)
      cSym.setInfo(NativeScalar).setFlag(SYNTHETIC)
      cSym.setFlag(if (isMutable) MUTABLE else FINAL)
      n -> Ident(cSym)
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
      sc.deScalarize(components)
    }

  }

  // Scalarize for specific coordinate (X,Y,A11,A12 etc.)
  def scalar(v: Tree, coord: Name) = v match {
    case Ident(name) if scope.inlinedVar(name).isDefined =>
      scope(name).scalar(coord)
    case _ => Select(v, coord)
  }

  // Descaralrize an escaping variable. Ex. new Vector2(v$x, v$y)
  def deScalarize(v: Tree) = v match {
    case Ident(name) if scope.inlinedVar(name).isDefined =>
      scope(name).deScalarize
    case _ => v
  }

}
