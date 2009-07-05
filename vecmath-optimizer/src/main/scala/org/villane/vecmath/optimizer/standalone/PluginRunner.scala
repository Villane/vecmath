package org.villane.vecmath.optimizer.standalone

import scala.tools.nsc.{Global, Settings, SubComponent}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}

/** This class is a compiler that will be used for running
 *  the plugin in standalone mode.
 */
class PluginRunner(settings: Settings, reporter: Reporter)
  extends Global(settings, reporter) {
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))

  //val optimizer = new VecMathOptimizer(PluginRunner.this)

  /*override def computePhaseDescriptors = {
    optimizer.components ::: super.computePhaseDescriptors
  }*/
}
