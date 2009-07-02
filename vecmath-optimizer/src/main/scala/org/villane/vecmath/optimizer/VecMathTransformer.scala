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

  class VMTransformer(cu: CompilationUnit) extends TypingTransformer(cu) {
    def typed(tr: Tree) = try {localTyper.typed(tr)} catch { case e =>
      println("error:" + tr)
      throw e
    }
    def typed(tr: Tree, mode: Int, tp: Type) = localTyper.typed(tr, mode, tp)

    /** When using <code>preTransform</code>, each node is visited before its children. */
    /*def preTransform(tree: Tree): Tree = tree match {
      case tr @ Apply(plus @ Select(rcvr, nme.PLUS), List(Literal(Constant(2f)))) =>
        println("PreTransf: " + rcvr + " : " + rcvr.getClass)
        copy.Apply(tree,
                   copy.Select(plus, rcvr, nme.PLUS),
                   List(typed(Literal(Constant(3f)))))
      case _ => tree
    }*/

    /** When using <code>postTransform</code>, each node is visited after its children. */
    /*def postTransform(tree: Tree): Tree = tree match {
      case tr @ Apply(plus @ Select(rcvr, nme.PLUS), List(Literal(Constant(2f)))) =>
        println("PostTransf: " + tree)
        //atPos(tree.pos)(Apply(Select(rcvr, nme.PLUS), List(Literal(Constant(5f)))))
        tree
      case _ => tree
    }*/

    def vectorToScalar(tree: Tree, coord: Name): Tree = tree match {
      case Apply(Select(v2, _), List(a)) =>
        new V2Scalarizer(cu, coord).transform(tree)
      case Select(v2, _) =>
        new V2Scalarizer(cu, coord).transform(tree)
      case Ident(v2) =>
        scalar(tree, coord)
      case _ => throw new BackOut(tree.toString)
    }

    /**---------------------
     * MAIN TRANSFORM METHOD
     * ---------------------*/
    override def transform(tree: Tree): Tree = tree match {
      case tr @ ClassDef(mods,_,_,_) if (!shouldOptimize(tr)) => tr
      case tr @ ModuleDef(mods,_,_) if (!shouldOptimize(tr)) => tr

      case tr @ DefDef(mods, name, a1, a2, a3, rhs) if (shouldOptimize(tr)) =>
        meth = tr
        println("optimizing def " + name + " {")
        println("--- ORIGINAL ---")
        println(meth)
        try {
          val res = typed(new Flattener(cu).transform(super.transform(rhs)))
          resetMeth
          val r = typed(copy.DefDef(tr, mods, name, a1, a2, a3, res))
          println("--- OPTIMIZED  ---")
          println(r)
          println("--- END ---")
          println("} //" + name)
          r
        } catch {
          // TODO backout must be method level
          case bo @ BackOut(msg) =>
            resetMeth
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
        }

      case tr @ ValDef(mods, name, V2(), rhs) if meth != null =>
        val iv = IV2(tr)
        try {
          inlinedVs(name) = iv
          println("RHS=" + rhs)
          val xv = typed(ValDef(iv.xn, vectorToScalar(rhs, X)))
          val yv = typed(ValDef(iv.yn, vectorToScalar(rhs, Y)))
          val stats = preTemps.toList ::: List(xv, yv)
          preTemps.clear
          Sequence(stats)
        } catch {
          case bo: BackOut =>
            iv.backedOut = true
            bo.iv = Some(iv)
            throw bo
        }

      case tr @ ValDef(mods, name, F(), rhs) if meth != null =>
        println("FLOAT RHS=" + rhs)
        val res = typed(new ScalarScalarizer(cu).transform(rhs))
        val r = typed(copy.ValDef(tr, mods, name, F(), res))
        val stats = preTemps.toList ::: List(r)
        preTemps.clear
        Sequence(stats)

      case tr @ Assign(Ident(name), rhs) if meth != null && inlinedVs.contains(name) =>
        val iv = inlinedVs(name)
        try {
          val xa = typed(Assign(Ident(iv.xn), vectorToScalar(rhs, X)))
          val ya = typed(Assign(Ident(iv.yn), vectorToScalar(rhs, Y)))
          val stats = preTemps.toList ::: List(xa, ya)
          preTemps.clear
          if (iv.isMutable && iv.lengthN.isDefined) iv.lengthDirty = true
          Sequence(stats)
        } catch {
          case bo: BackOut =>
            iv.backedOut = true
            bo.iv = Some(iv)
            throw bo
        }

      // Apply replacements v.x -> v$x and so on
      case Select(v @ Ident(name), op) if (inlinedVs contains name) =>
        if (op == X || op == Y)
          typed(scalar(v, op))
        else
          new ScalarScalarizer(cu).transform(tree)

      case tr @ Ident(name) if (inlinedVs contains name) =>
        // TODO only do this if the IV2 is escaping!!!
        deScalar(tr)

        /*
      case tr @ Apply(plus @ Select(rcvr, nme.PLUS), List(Literal(Constant(v: Float)))) =>
        println("PreTransf: " + rcvr + " : " + rcvr.getClass)
        copy.Apply(tree,
                   copy.Select(plus, rcvr, nme.PLUS),
                   List(typed(Literal(Constant(v + 2)))))
        */

      case _ =>
        if (meth != null) {
          println(tree.getClass)
          println(tree)
        }
        super.transform(tree)
    }
  }

  class Flattener(cu: CompilationUnit) extends TypingTransformer(cu) {
    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) =>
        println("FLATTENING")
        val stats2 = new collection.mutable.ListBuffer[Tree]
        stats foreach { stat => stat match {
          case Sequence(ls) => stats2 ++= ls
          case _ => stats2 += stat
        }}
        val expr2 = expr match {
          case Sequence(ls) => throw new Error("should not happen")
          case _ => expr
        }
        Block(stats2.toList, expr2)
      case _ => tree
    }
  }
}
