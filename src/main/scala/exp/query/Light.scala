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

package exp.query

import al.strategies._
import app.ArgParser
import app.db.entities.Dataset
import exp.CrossValidation
import ml.Pattern
import weka.filters.unsupervised.attribute.Standardize

object Light extends CrossValidation with App {
  val args1 = args
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded light GNOSTIC " +
    "strategies (i.e. not Rnd, Clu and not EER) mostly due to the fact that they are fast and don't need to be stopped earlier;\n"
  val (path, datasetNames0, learner) = ArgParser.testArgsWithLearner(className, args, desc)

  run(ff)

  def strats0(run: Int, pool: Seq[Pattern]) = List(
    Uncertainty(learner(run, pool), pool),
    Entropy(learner(run, pool), pool),
    Margin(learner(run, pool), pool),
    new SGmulti(learner(run, pool), pool, "consensus"),
    new SGmulti(learner(run, pool), pool, "majority"),
    new SGmultiJS(learner(run, pool), pool),
    DensityWeighted(learner(run, pool), pool, 1, "eucl"),
    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "cheb"),
    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "eucl"),
    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "maha"),
    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "manh"),
    MahalaWeighted(learner(run, pool), pool, 1),
    MahalaWeightedTrainingUtility(learner(run, pool), pool, 1, 1)
  )

  def ee(db: Dataset) = {
    val fazer = !db.isLocked() && (if (!completeForQCalculation(db)) {
      println(s"$db is not Rnd queries/hits complete to calculate Q. Skipping...")
      false
    } else if (!nonRndQueriesComplete(db)) true
    else {
      println(s"Light queries are complete for $db with ${learner(-1, Seq())}.")
      false
    })
    fazer
  }

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize) {
    val Q = q(db)
    strats(run, pool).takeWhile { strat =>
      db.saveQueries(strat, run, fold, f, timeLimitSeconds, Q)
      db.running
    }
  }
}
