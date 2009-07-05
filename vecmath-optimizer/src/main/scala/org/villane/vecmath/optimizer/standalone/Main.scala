package org.villane.vecmath.optimizer.standalone

import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings

object Main {

  def main(args: Array[String]) {
    val settings = new Settings

    val command = new CompilerCommand(args.toList, settings, println, false)

    if (!command.ok)
      return()

    if (settings.help.value) {
      println(command.usageMsg)
      return()
    }

    val runner = new PluginRunner(settings)
    val run = new runner.Run
    run.compile(command.files)
  }

}
