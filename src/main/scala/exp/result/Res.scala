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

package exp.result

import al.strategies._
import app.ArgParser
import app.db.ClassName
import app.db.entities.{AppFile, Dataset}
import ml.classifiers.Learner

trait Res extends App with ClassName {
  lazy val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  lazy val parallel = {
    learner //<- just to unlazy argsTest and avoid OutOfBounds Exc here
    args(2) == "y"
  }
  val samplingSize = 500
  val runs = Dataset("")("").runs
  val folds = Dataset("")("").folds
  val desc: String
  val readOnly: Boolean
  lazy val af = {
    val r = AppFile(createOnAbsence = false, readOnly = true)
    r.open()
    r
  }
  val medida: String
  val learners: Seq[Learner]
  lazy val mid = af.fetchmid(medida)

  def fetsid(s: Strategy) = af.fetchsid(s)

  def fetchlid(l: Learner) = af.fetchlid(l)

  def core(db: Dataset, sid: Int, Q: Int, st: String, le: String, lid: Int): Boolean

  lazy val strats = List(
    RandomSampling(Seq()),
    //    ClusterBased(Seq()),
    //
    //    Entropy(learner(-1, Seq()), Seq()),
    //    Margin(learner(-1, Seq()), Seq()),
    //    new SGmulti(learner(-1, Seq()), Seq(), "consensus"),
    //    new SGmulti(learner(-1, Seq()), Seq(), "majority"),
    //    new SGmultiJS(learner(-1, Seq()), Seq()),
    //    DensityWeighted(learner(-1, Seq()), Seq(), 1, "eucl"),
    //    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "cheb"),
    //    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "eucl"),
    //    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "maha"),
    //    DensityWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1, "manh"),
    //    MahalaWeighted(learner(-1, Seq()), Seq(), 1),
    //    MahalaWeightedTrainingUtility(learner(-1, Seq()), Seq(), 1, 1),

    ExpErrorReduction(learner(-1, Seq()), Seq(), "entropy", samplingSize),
    ExpErrorReductionMargin(learner(-1, Seq()), Seq(), "entropy", samplingSize),
    ExpErrorReduction(learner(-1, Seq()), Seq(), "accuracy", samplingSize),
    ExpErrorReductionMargin(learner(-1, Seq()), Seq(), "gmeans+residual", samplingSize)
    //    ,
    //    SVMmulti(Seq(), "SELF_CONF"),
    //    SVMmulti(Seq(), "KFF"),
    //    SVMmulti(Seq(), "BALANCED_EE"),
    //    SVMmulti(Seq(), "SIMPLE")
  )

  def end(): Unit

  def run() {
    (if (parallel) datasetNames.par else datasetNames).toList map { datasetName =>
      val db = Dataset(path, createOnAbsence = false, readOnly)(datasetName)
      db.open()
      if (db.isOpen()) {
        val Q = db.Q
        strats foreach { st =>
          learners foreach { le =>
            val lid = fetchlid(le)
            val sid = fetsid(st)
            val medidas = db.exec(s"select count(*) from res where m=$mid and s=$sid and l=$lid").get.head.head
            val complete = medidas == runs * folds
            if (readOnly) {
              //tex
              if (core(db, sid, Q, st.abr.toString, le.toString, lid) && complete) println(s"$db / $st : ok")
              else {
                println(s"$db / $st / $le : collecting of results incomplete!")
              }
            } else {
              //res
              if (complete) println(s"$medida already calculated for $db / $st.")
              else {
                if (medidas > runs * folds) db.safeQuit(s"Inconsistency: $medidas ${medida}s is greater than ${runs * folds} pools!")
                else {
                  db.exec("begin")
                  if (core(db, sid, Q, st.abr.toString, le.toString, lid)) println(s"$db / $st : ok")
                  db.exec("end")
                }
              }
            }
          }
        }
        if (!readOnly) db.save()
        db.close()
      }
    }
    af.close()
    end()
    println("bye!")
  }
}
