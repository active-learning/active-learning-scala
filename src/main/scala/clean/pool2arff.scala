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

package clean

import java.io.FileWriter

import ml.Pattern
import weka.filters.Filter

object pool2arff extends Exp with LearnerTrait with StratsTrait {
   val context = "pool2arffApp"
   val arguments = superArguments ++ Seq("normalizar:y|n")
   val ignoreNotDone = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      val exemplos = if (normalizar) fpool else pool
      val header = exemplos.head.dataset().toString.split("\n").takeWhile(!_.contains("@data")).mkString("\n")
      val data = exemplos.mkString("\n")
      val fw = new FileWriter(s"$ds-r$run-f$fold-${if (normalizar) "normalized-" else ""}pool.arff")
      fw.write(header + "\n@data\n" + data)
      fw.close()
      //      println(header + "\n@data\n" + data)
   }

   def datasetFinished(ds: Ds) {
   }

   def isAlreadyDone(ds: Ds) = false

   def end(res: Map[String, Boolean]): Unit = {
   }
}
