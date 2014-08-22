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

package exp

import al.strategies._
import app.ArgParser
import app.db.ClassName
import app.db.entities.Dataset
import ml.Pattern

trait Res extends App with ClassName {
  lazy val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  lazy val parallel = args(2) == "y"
  lazy val dest = Dataset(path, createOnAbsence = false, readOnly) _
  val samplingSize = 500
  val readOnly = true
  val runs = 5
  val folds = 5
  val desc: String

  def core(db: Dataset)

  def strats(run: Int, pool: Seq[Pattern]) = List(
    RandomSampling(pool),
    ClusterBased(pool),

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
    MahalaWeightedTrainingUtility(learner(run, pool), pool, 1, 1),

    ExpErrorReduction(learner(run, pool), pool, "entropy", samplingSize),
    ExpErrorReductionMargin(learner(run, pool), pool, "entropy", samplingSize),
    ExpErrorReduction(learner(run, pool), pool, "accuracy", samplingSize),
    ExpErrorReduction(learner(run, pool), pool, "gmeans", samplingSize),

    SVMmulti(pool, "SELF_CONF"),
    SVMmulti(pool, "KFF"),
    SVMmulti(pool, "BALANCED_EE"),
    SVMmulti(pool, "SIMPLE")
  )

  def run() {
    (if (parallel) datasetNames.par else datasetNames).toList map { datasetName =>
      val db = dest(datasetName)
      db.open()
      core(db)
      db.close()
    }
  }
}
