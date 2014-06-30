package exp.other

import app.ArgParser
import app.db.{Dataset, Lock, Results}
import exp.raw.{ClassName, CrossValidation}
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
object ComparingClassifiers extends CrossValidation with App with Lock {
  println("First experiment:")
  println("teaching-assistant-evaluation,wine,statlog-heart,flare,molecular-promotor-gene,leukemia-haslinger,balance-scale,pima-indians-diabetes,car-evaluation,breast-cancer-wisconsin,wine-quality-red,connectionist-mines-vs-rocks,cmc,connectionist-vowel,monks1,breast-tissue-6class,ionosphere,dbworld-subjects-stemmed,statlog-australian-credit,thyroid-newthyroid,colon32,hayes-roth,dbworld-bodies-stemmed,statlog-vehicle-silhouettes,acute-inflammations-urinary,iris,yeast-4class,tic-tac-toe")
  println("sqlite3 -header res.db \"attach 'app.db' as app; select l.name as le, learnerid, round(avg(accuracy), 3) as m, datasetid, time, d.name from ComparingClassifiers as c, app.learner as l, app.dataset as d where d.rowid=datasetid and l.rowid=learnerid group by learnerid, datasetid order by le, m;\"" + " | sed -r 's/[\\|]+/\t/g'")
  val desc = "Version " + ArgParser.version + " \n 5-fold CV for C4.5 VFDT 5-NN NB interaELM ELM-sqrt\n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path, createOnAbsence = false, readOnly = true) _

  //warming ELMs up
  val warmingdata = Datasets.arff(bina = true)("banana.arff") match {
    case Right(x) => x
    case Left(str) => println("Could not load banana dataset from the program path: " + str); sys.exit(0)
  }
  CIELM(5).build(warmingdata).accuracy(warmingdata)


  val resultsDb = Results(createOnAbsence = true)
  if (resultsDb.open(debug = true) || resultsDb.run(s"select count(*) from sqlite_master WHERE type='table' AND name='$className'").left.get == 0)
    resultsDb.run(s"create table $className (datasetid INT, learnerid INT, run INT, fold INT, accuracy FLOAT, time FLOAT, unique(datasetid, learnerid, run, fold) on conflict rollback)")
  resultsDb.save()
  run()
  resultsDb.save()
  resultsDb.close()

  def runCore(db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: => Seq[Pattern]) {
    val name = db.database
    val did = resultsDb.run(s"select rowid from app.dataset where name = '$name'").left.get
    val learners = Seq(IELM(pool.size), EIELM(pool.size), CIELM(pool.size),
      interaELM(math.min(300, pool.size / 3)), OSELM(math.sqrt(pool.size).toInt),
      VFDTBatch(), NBBatch(), KNNBatch(5, "eucl"), C45(), ECIELM(pool.size))

    //Heavy processing.
    val results = learners map { learner =>
      val lid = resultsDb.run(s"select rowid from app.learner where name = '$learner'").left.get
      val previous = resultsDb.run(s"select count(*) from $className where datasetid=$did and learnerid=$lid and run=$run and fold=$fold").left.get
      if (previous > 0) {
        println(s"Results already done previously for $name , skipping it.")
        None
      } else {
        val (m, t) = Tempo.timev(learner.build(pool))
        val acc = m.accuracy(testSet)
        println(s"Run $run, pool $fold: $learner finished for $name !")
        Some(acc, t, lid)
      }
    }

    //Just collecting results.
    acquire()
    resultsDb.run("begin")
    results.flatten foreach { case (acc, t, lid) =>
      resultsDb.run(s"insert into $className values ($did, $lid, $run, $fold, $acc, $t)")
    }
    resultsDb.run("end")
    if (fold == 4 && run == 4) resultsDb.save()
    release()
  }
}

object TableForComparingClassifiers extends App with ClassName {
  println("learner \taccur.  \ttime\tdataset\n---------------------------------------")
  val resultsDb = Results(createOnAbsence = false, readOnly = true)
  resultsDb.open()
  val fields = "l.name as le, round(avg(accuracy), 3) as m, round(sum(time),3), d.name, count(*)"
  val sources = s"${className.drop(8)} as c, app.learner as l, app.dataset as d"
  val conditions = "d.rowid=datasetid and l.rowid=learnerid"
  val aggregators = "group by learnerid, datasetid order by le, m;\""
  val q = resultsDb.runStr(s"select $fields from $sources where $conditions $aggregators").right.get
  val g = q.groupBy(seq => seq.dropRight(1).last)
  for ((dataset, queue) <- g) {
    println(dataset)
    val pad = queue.map(_.head.size).max
    val max = queue.map(_(1)).max
    val min = queue.map(_(1)).min
    for (seq <- queue) {
      if (seq.last.toInt != 25) {
        println(s"Incomplete results: folds*runs=${seq.last}")
        sys.exit(0)
      }
      if (seq(1) == max) print("+")
      else {
        if (seq(1) == min) print("-") else print(" ")
      }
      println(seq.dropRight(1).map(_.padTo(pad, " ").mkString).mkString("|"))
    }
    println("")
  }
  resultsDb.close()
}