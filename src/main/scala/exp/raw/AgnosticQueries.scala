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
import weka.filters.unsupervised.attribute.Standardize

object AgnosticQueries extends CrossValidation with App {
  val args1 = args
  val desc = "Version " + ArgParser.version + " \n Generates queries for the given list of datasets according to provided hardcoded agnostic " +
    "strategies (Rnd and Clu) mostly due to the fact that both should go until the end;\n" +
    "Rnd because it is the baseline to define Q and\n" +
    "Clu because it relies on external implementation.\n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val dest = Dataset(path) _
  run(ff)

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize) {
    db.saveQueries(RandomSampling(pool), run, fold, f, 3600) //it is interesting to have all queries, but we have to save sometimes.
    db.saveQueries(ClusterBased(pool), run, fold, f, Int.MaxValue) //a small time limit would discard all the Cluster queries.
  }
}
