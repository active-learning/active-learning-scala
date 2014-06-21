/*
 active-learning-scala: Active Learning library for Scala
 Copyright (c) 2014 Davi Pereira dos Santos

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package ml.generators

import java.io.{File, FileWriter}

import app.ArgParser
import weka.core.Instances

object ARFF2ARFF extends App {
  println("Version " + 0.7 + ".")


  val (path ,names)= ArgParser.testArgs(getClass.getSimpleName.dropRight(1), args, 3,"Version " + 0.7 + "." +
    "Preprocessing for ARFF files. It is needed to apply z-score for use in ELMs.\n" +
    "It does not preserve the order of the instances.\nSteps:\n RemoveUseless attributes\n" +
    " Nominal -> Binary (numeric)\n" +
    " deduplicate (keeps only one instance with the mode of the conflicting labels).")

  def f(name: String, instancesOp: => Option[Instances]) {
    val file = new File(path + name + ".processed.arff")
    if (file.exists) {
      //TODO: Test beyond the emptyness of arff file.
      if (file.length == 0) {
        file.delete
        println("Deleted empty file '" + path + name + ".processed.arff'.")
      }
    }
    if (!file.exists && instancesOp.isDefined) {
      val instances = instancesOp.get
      val fw = new FileWriter(path + name + ".processed.arff", false)
      fw.write(instances.toString)
      fw.close()
      println(" Finished: '" + name + "'.")
    }
  }

  ArgParser.applyArgs(args)(f)
}