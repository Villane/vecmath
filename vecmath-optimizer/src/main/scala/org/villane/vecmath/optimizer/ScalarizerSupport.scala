package org.villane.vecmath.optimizer

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._

trait ScalarizerSupport { self: VecMathOptimizer =>
  import global._
  import definitions._

  /** Meta-data about scalarizable classes. */
  trait Scalarizable {
    type ClassType <: AnyRef
    def compObject: Option[Symbol]
    def classType: Type
    def constants: Map[Name, ClassType]
    def creators: Map[Name, (Seq[Tree], Name) => Tree]
    def scalarComponents: List[Name]
    def scalarValue(obj: ClassType, name: Name): Float
    def isObjectOf(tpe: Type) = compObject match {
      case Some(obj) => tpe == obj.tpe || tpe.widen == obj.tpe
      case None => false
    }
    def isClassOf(tpe: Type) = tpe == classType || tpe.widen == classType
    def isTypeOf(tree: Tree) = tree.tpe == classType || tree.tpe.widen == classType

    def deScalarize(args: collection.Map[Name, Tree]) =
      // By default we expect the cons to take scalar arguments in order
      New(TypeTree(classType), List(scalarComponents map args))

    def componentFromArgs(args: List[Tree], comp: Name): Tree
    def newNormalVar(vDef: ValDef): NormalVariable
    def newScalarizedVar(vDef: ValDef): ScalarizedVariable

    def optimizeSelect(tree: Select): Tree
  }

  // Untyped
  object UTBinOp {
    def apply(l: Tree, op: Name, r: Tree) = Apply(Select(l, op), List(r))
    def unapply(tr: Tree) = tr match {
      case Apply(Select(l, op), List(r)) => Some(l, op, r)
      case _ => None
    }
  }

  // Untyped
  object UTUnOp {
    def apply(l: Tree, op: Name) = Select(l, op)
    def unapply(tr: Tree) = tr match {
      case Select(l, op) => Some(l, op)
      case _ => None
    }
  }

  trait TransformerSupport { self: VMTransformer =>

    object ScalarizableObject {
      def unapply(tpe: Type): Option[Scalarizable] = tpe match {
        case V2Object() => Some(V2Scalarizable)
        case M22Object() => Some(M22Scalarizable)
        case _ => None
      }
      def unapply(tr: Tree): Option[Scalarizable] = unapply(tr.tpe)
    }

    def isScalarizable(tr: Tree) = Scalarizable.unapply(tr).isDefined

    /** Matches Types (or Types of Trees) that are Scalarizable */
    object Scalarizable {
      def unapply(tpe: Type): Option[Scalarizable] = tpe match {
        case V2() => Some(V2Scalarizable)
        case M22() => Some(M22Scalarizable)
        case _ => None
      }
      def unapply(tr: Tree): Option[Scalarizable] = unapply(tr.tpe)
    }

    object SConstant {
      def unapply(tr: Tree) = tr match {
        case Select(ScalarizableObject(sc), name) 
          if sc.constants contains name => Some(sc, name)
        case _ => None
      }
    }

    object SCreator {
      def unapply(tr: Tree) = tr match {
        case Apply(Select(ScalarizableObject(sc), name), args) 
          if sc.creators contains name => Some(sc, name, args)
        case _ => None
      }
    }

    object SComponent {
      def unapply(tr: Tree) = tr match {
        case Select(sel @ Scalarizable(sc), name)
          if sc.scalarComponents contains name => Some(sel, name)
        case _ => None
      }
    }

    object BinOp {
      def apply(l: Tree, op: Name, r: Tree) = typed(Apply(Select(l, op), List(r)))
      def unapply(tr: Tree) = tr match {
        case Apply(Select(l, op), List(r)) => Some(l, op, r)
        case _ => None
      }
    }

    object UnOp {
      def apply(l: Tree, op: Name) = typed(Select(l, op))
      def unapply(tr: Tree) = tr match {
        case Select(l, op) => Some(l, op)
        case _ => None
      }
    }

    object MathFun {
      def apply(math: Ident, op: Name, args: Tree*) = typed(Apply(Select(math, op), args.toList))
      def unapplySeq(tr: Tree) = tr match {
        case Apply(Select(math, op), args) => Some(math, op, args.toSeq)
        case _ => None
      }
    }

    object NewObj {
      def apply(tpt: TypeTree, args: Tree*) = typed(Apply(Select(New(tpt), nme.CONSTRUCTOR), args.toList))
    }

    // TODO needs to be typed?
    def scalarizeConstant(scalarizable: Scalarizable, name: Name, comp: Name) =
      typed(Literal(scalarizable.scalarValue(scalarizable.constants(name), comp)))

    def scalarizeCreator(scalarizable: Scalarizable, name: Name, args: Seq[Tree], comp: Name) =
      scalarizable.creators(name)(args, comp)

  }

}