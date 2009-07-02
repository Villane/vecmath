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

  case class BackOut(msg: String) extends Throwable(msg) {
    var iv: Option[IV2] = None
  }

  /* inlined variable, actually scalar replaced variable */
  sealed abstract class Inlined(val vDef: ValDef) {
    def isMutable = vDef.mods.isVariable
    def scalar(coord: Name): TermSymbol
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
    preTemps += typed(ValDef(iv.xn, vectorToScalar(tree, X)))
    preTemps += typed(ValDef(iv.yn, vectorToScalar(tree, Y)))
  }*/

  /** inlined vector2 */
  case class IV2(override val vDef: ValDef) extends Inlined(vDef) {
    def scalar(coord: Name) = {
      usageCount += 1
      coord match {
        case X =>
          xUsageCount += 1
          xn
        case Y =>
          yUsageCount += 1
          yn
      }
    }

    val xn = vDef.symbol.newValue(vDef.pos, vDef.name + "$x")
    val yn = vDef.symbol.newValue(vDef.pos, vDef.name + "$y")
    var xUsageCount = 0
    var yUsageCount = 0

    xn.setInfo(FT)
    yn.setInfo(FT)
    if (vDef.mods.isVariable) {
      xn.setFlag(MUTABLE)
      yn.setFlag(MUTABLE)
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
      val lenSqr = Apply(
        Select(Apply(Select(Ident(xn), nme.MUL), List(Ident(xn))),
        nme.ADD),
        List(Apply(Select(Ident(yn), nme.MUL), List(Ident(yn))))
      )
      val vLen = Assign(Ident(ln), typer.typed(Apply(Select(VecMath, Sqrt), List(lenSqr))))
      preTemps += vLen
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
      val lenSqr = Apply(
        Select(Apply(Select(Ident(xn), nme.MUL), List(Ident(xn))),
        nme.ADD),
        List(Apply(Select(Ident(yn), nme.MUL), List(Ident(yn))))
      )
      val vLen = ValDef(ln, typer.typed(Apply(Select(VecMath, Sqrt), List(lenSqr))))
      preTemps += vLen
      vLen
    }
  }

  // statements inserted !before! the current scalarization being done
  val preTemps = new collection.mutable.ListBuffer[Tree]
  var _genV2 = Stream.from(0)
  def nextV2 = {
    val n = _genV2.head
    _genV2 = _genV2.tail
    meth.symbol.newValue(meth.pos, "$anon$v" + n)
  }

  // TODO Stack of blocks!!!
  var meth: DefDef = null
  // inlined Vectors per method
  val inlinedVs = collection.mutable.Map[Name,IV2]()
  val backoutVs = collection.mutable.Map[Name,IV2]()
  def resetMeth() {
    meth = null
    inlinedVs.clear
    backoutVs.clear
    preTemps.clear
    _genV2 = Stream.from(0)
  }

  // Scalarize for specific coordinate (X | Y)
  def scalar(v: Tree, coord: Name) = v match {
    case Ident(name) if inlinedVs.contains(name) =>
      Ident(inlinedVs(name).scalar(coord))
    case _ => Select(v, coord)
  }

  // Descaralrize an escaping variable. Ex. new Vector2(v$x, v$y)
  def deScalar(v: Tree) = v match {
    case Ident(name) if inlinedVs.contains(name) =>
      val iv = inlinedVs(name)
      iv.deScalarCount += 1
      typer.typed(New(V2(), List(List(Ident(iv.xn), Ident(iv.yn)))))
    case _ => v
  }

}
