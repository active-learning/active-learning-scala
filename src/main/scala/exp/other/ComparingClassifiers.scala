package exp.other

import app.ArgParser
import app.db.{Results, AppFile, Dataset}
import exp.raw.CrossValidation
import ml.Pattern
import ml.classifiers._
import util.{Datasets, Tempo}

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
  println("teaching-assistant-evaluation,wine,statlog-heart,flare,molecular-promotor-gene,leukemia-haslinger,balance-scale,pima-indians-diabetes,car-evaluation,breast-cancer-wisconsin,wine-quality-red,connectionist-mines-vs-rocks,cmc,connectionist-vowel,monks1,breast-tissue-6class,ionosphere,dbworld-subjects-stemmed,statlog-australian-credit,thyroid-newthyroid,colon32,hayes-roth,dbworld-bodies-stemmed,statlog-vehicle-silhouettes,acute-inflammations-urinary,iris,yeast-4class,tic-tac-toe")
  println("")
  val desc = "Version " + ArgParser.version + " \n 5-fold CV for C4.5 VFDT 5-NN NB interaELM ELM-sqrt\n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _

  //warming ELMs up
  val warmingdata = Datasets.arff(bina = true)("banana.arff") match {
    case Right(x) => x
    case Left(str) => println("Could not load banana dataset from the program path: " + str); sys.exit(0)
  }
  CIELM(5).build(warmingdata).accuracy(warmingdata)


  val resultsDb = Results(create = true)
  resultsDb.open(debug = true)
  resultsDb.run(s"create table $className (datasetid INT, learnerid INT, run INT, fold INT, accuracy FLOAT, time FLOAT, unique(datasetid, learnerid, run, fold) on conflict rollback)")
  resultsDb.save()
  run()
  resultsDb.close()

  def runCore(db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: => Seq[Pattern]) {
    val learners = Seq(IELM(pool.size), EIELM(pool.size), CIELM(pool.size), interaELM(pool.size / 3), OSELM(math.sqrt(pool.size).toInt), HT(), NB(), KNN(5, "eucl"))
    learners foreach { learner =>
      val lid = resultsDb.run(s"select rowid from app.learner where name = '$learner'").left.get
      val did = resultsDb.run(s"select rowid from app.dataset where name = '${db.database}'").left.get
      val previous = resultsDb.run(s"select count(*) from $className where datasetid=$did and learnerid=$lid and run=$run and fold=$fold").left.get
      if (previous > 0) {
        println("Results already done previously, skipping it.")
      } else {
        val (m, t) = Tempo.timev(learner.build(pool))
        val acc = m.accuracy(testSet)
        resultsDb.run(s"insert into $className values ($did, $lid, $run, $fold, $acc, $t)")
      }
    }
    if (fold == 4) resultsDb.save()
  }
}