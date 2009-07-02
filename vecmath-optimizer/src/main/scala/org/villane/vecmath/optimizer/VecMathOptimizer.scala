package org.villane.vecmath.optimizer

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class VecMathOptimizer(val global: Global) extends Plugin {

  val name = "vecmathopt"
  val description = "optimizes code that uses org.villane.vecmath"
  val components = List[PluginComponent](new VecMathTransformer(global))

  /*private object Component extends PluginComponent with Transform {
    val global: VecMathOptimizer.this.global.type = VecMathOptimizer.this.global
    val runsAfter = "refchecks"
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    // val runsAfter = List[String]("refchecks");
    val phaseName = VecMathOptimizer.this.name
    def newPhase(_prev: Phase) = new VecMathOptimizerPhase(_prev)    
    
    class VecMathOptimizerPhase(prev: Phase) extends StdPhase(prev) {
      def apply(unit: global.CompilationUnit) {
        println(unit.source.path)
      }
    }
  }*/
}
