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

}
