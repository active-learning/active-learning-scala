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

package exp.raw

import al.strategies.{ClusterBased, RandomSampling}
import app.ArgParser
import app.db.Dataset
import ml.Pattern
import util.Datasets

/**
 * Created by davi on 05/06/14.
 */
object AgnosticQueries extends CrossValidation with App {
  val runs = 5
  val folds = 5
  val desc = "Version " + ArgParser.version + " \n Generates queries for the given list of datasets according to provided hardcoded agnostic " +
    "strategies (Rnd and Clu) mostly due to the fact that both should go until the end;\n" +
    "Rnd because it is the baseline to define Q and\n" +
    "Clu because it relies on external implementation.\n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _

  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern]) =>
    db.saveQueries(RandomSampling(pool), run, fold)
    db.saveQueries(ClusterBased(pool), run, fold)
  }
}
