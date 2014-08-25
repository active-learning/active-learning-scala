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
import app.db.entities.{AppFile, Dataset}
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
  lazy val af = {
    val r = AppFile(createOnAbsence = false, readOnly = true)
    r.open()
    r
  }
  lazy val lid = af.fetchlid(learner(-1, Seq()))

  def sid(s: Strategy) = af.fetchsid(s)

  def core(db: Dataset)

  lazy val strats = List(
    RandomSampling(Seq()),
    ClusterBased(Seq()),

    Entropy(learner(-1, Seq()), Seq()),
    Margin(learner(-1, Seq()), Seq()),
    new SGmulti(learner(-1, Seq()), Seq(), "consensus"),
    new SGmulti(learner(-1, Seq()), Seq(), "majority"),
    new SGmultiJS(learner(-1, Seq()), Seq()),
    DensityWeighted(learner(-1, Seq()), Seq(), 1, "eucl"),
    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "cheb"),
    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "eucl"),
    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "maha"),
    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "manh"),
    MahalaWeighted(learner(-1, Seq()), Seq(), 1),
    MahalaWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1),

    ExpErrorReduction(learner(-1, Seq()), Seq(), "entropy", samplingSize),
    ExpErrorReductionMargin(learner(-1, Seq()), Seq(), "entropy", samplingSize),
    ExpErrorReduction(learner(-1, Seq()), Seq(), "accuracy", samplingSize),
    ExpErrorReduction(learner(-1, Seq()), Seq(), "gmeans", samplingSize),

    SVMmulti(Seq(), "SELF_CONF"),
    SVMmulti(Seq(), "KFF"),
    SVMmulti(Seq(), "BALANCED_EE"),
    SVMmulti(Seq(), "SIMPLE")
  )

  def run() {
    (if (parallel) datasetNames.par else datasetNames).toList map { datasetName =>
      val db = dest(datasetName)
      db.open()
      core(db)
      db.close()
    }
    af.close()
  }
}
