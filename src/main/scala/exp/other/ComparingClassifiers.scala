package exp.other

import app.ArgParser
import app.db.{AppFile, Dataset}
import exp.raw.CrossValidation
import ml.Pattern
import ml.classifiers._
import util.{Tempo, Datasets}

import scala.collection.mutable

/*
elm-scala: an implementation of ELM in Scala using MTJ
Copyright (C) 2014 Davi Pereira dos Santos

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
object ComparingClassifiers extends CrossValidation with App {
  println("First experiment:")
  println("tae,wine,statlog-heart,flare,molecular-promoters,leukemia-haslinger,balance-scale,pima,car,breast-cancer-wisconsin-diagnostic,wine-quality-red,connectionist-mines-vs-rocks,cmc,vowel,monk1,breast-tissue,ionosphere,subject,australian,newthyroid,colon32,hayes-roth,bodies,vehicle,accute-inflammations,iris,yeast-4classes,tic-tac-toe")
  println("")
  val desc = "Version " + ArgParser.version + " \n 5-fold CV for C4.5 VFDT 5-NN NB interaELM ELM-sqrt\n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _

  val af = AppFile()
  af.open(debug = true)
  af.run(s"create table $className (learnerid INT, fold INT, accuracy FLOAT, time FLOAT, unique(learnerid, fold))")
  run()
  af.close()

  def runCore(db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: => Seq[Pattern]) {
    val learners = Seq(IELM(pool.size), EIELM(pool.size), CIELM(pool.size), interaELM(pool.size / 3), OSELM(math.sqrt(pool.size).toInt), HT(), NB(), KNN(5, "eucl"))
    val accs_ts = learners foreach { learner =>
      val lid = af.run(s"select rowid from learner where name = '$learner'")
      val previous = af.run(s"select count(*) from ${db.database} where learnerid=$lid and fold=$fold").left.get
      if (previous > 0) {
        println("Results already done previously, skipping it.")
      } else {
        val (m, t) = Tempo.timev(learner.build(pool))
        val acc = m.accuracy(testSet)
        af.run(s"insert into ${db.database} values ($lid, $acc, $t))")
      }
    }
  }
}