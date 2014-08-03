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
package exp.other

import app.ArgParser
import app.db.{Dataset, Results}
import exp.raw.{ClassName, CrossValidation}
import ml.Pattern
import ml.classifiers._
import ml.models.ELMModel
import util.{Datasets, Lock, Tempo}
import weka.filters.unsupervised.attribute.Standardize

import scala.util.Random

object ComparingBatchClassifiers extends CrossValidation with App with Lock {
  val args1 = args
  println("First experiment:")
  println("teaching-assistant-evaluation,wine,statlog-heart,flare,molecular-promotor-gene,leukemia-haslinger,balance-scale,pima-indians-diabetes,car-evaluation,breast-cancer-wisconsin,wine-quality-red,connectionist-mines-vs-rocks,cmc,connectionist-vowel,monks1,breast-tissue-6class,ionosphere,dbworld-subjects-stemmed,statlog-australian-credit,thyroid-newthyroid,colon32,hayes-roth,dbworld-bodies-stemmed,statlog-vehicle-silhouettes,acute-inflammations-urinary,iris,yeast-4class,tic-tac-toe")
  println("sqlite3 -header results.db \"attach 'app.db' as app; select l.name as le, learnerid, round(avg(accuracy), 3) as m, datasetid, time, d.name from ComparingClassifiers as c, app.learner as l, app.dataset as d where d.rowid=datasetid and l.rowid=learnerid group by learnerid, datasetid order by le, m;\"" + " | sed -r 's/[\\|]+/\t/g'")
  val desc = "Version " + ArgParser.version + " \n 5-fold CV for C4.5 VFDT 5-NN NB interaELM ELM-sqrt\n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val dest = Dataset(path, createOnAbsence = false, readOnly = true) _

  //warming ELMs up
  val warmingdata = Datasets.arff(bina = true)("banana.arff") match {
    case Right(x) => x
    case Left(str) => println("Could not load banana dataset from the program path: " + str); sys.exit(0)
  }
  CIELM(5).build(warmingdata).accuracy(warmingdata)


  val resultsDb = Results("/home/davi/wcs/ucipp/uci", createOnAbsence = true)
  if (resultsDb.open(debug = true) || resultsDb.exec(s"select count(*) from sqlite_master WHERE type='table' AND name='$className'").left.get == 0)
    resultsDb.exec(s"create table $className (datasetid INT, learnerid INT, run INT, fold INT, accuracy FLOAT, time FLOAT, unique(datasetid, learnerid, run, fold) on conflict rollback)")
  resultsDb.save()
  run(ff)

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize) {
    val name = db.database
    val did = resultsDb.exec(s"select rowid from app.dataset where name = '$name'").left.get
    val learners = Seq(IELM(pool.size), EIELM(pool.size), CIELM(pool.size), ECIELM(pool.size),
      interaELM(math.min(100, pool.size / 3)), interaELMNoEM(math.min(100, pool.size / 3)),
      interawELM(15), interawfELM(15),
      OSELM(math.sqrt(pool.size).toInt),
      VFDT(), NB(), KNN(5, "eucl", pool), C45())

    //Heavy processing.
    val results = learners map { learner =>
      val lid = resultsDb.exec(s"select rowid from app.learner where name = '$learner'").left.get
      val previous = resultsDb.exec(s"select count(*) from $className where datasetid=$did and learnerid=$lid and run=$run and fold=$fold").left.get
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

    println("Just collecting and storing results.")
    acquire()
    resultsDb.exec("begin")
    results.flatten foreach { case (acc, t, lid) =>
      resultsDb.exec(s"insert into $className values ($did, $lid, $run, $fold, $acc, $t)")
    }
    resultsDb.exec("end")
    if (fold % 3 == 0) resultsDb.save()
    release()
    println("collected!")
  }

  resultsDb.save()
  resultsDb.close()
}

object TableForComparingBatchClassifiers extends App with ClassName {
  println("learner \taccur.  \ttime\tdataset\n---------------------------------------")
  val resultsDb = Results("/home/davi/wcs/ucipp/uci", createOnAbsence = false, readOnly = true)
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
      if (seq.last.toInt != 10) {
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

object interasTest extends App {
  //  val data = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci")("banana").right.get)
  val patts0 = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("abalone-11class").right.get.value).take(2000)
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilter(patts0, filter)
  val pool = patts.take(1000)
  val ts = patts.drop(1000)
  val learners = Seq(IELM(pool.size), IELMScratch(pool.size), EIELM(pool.size), CIELM(pool.size),
    interaELM(math.min(100, pool.size / 3)), interawELM(15), interawfELM(15))
  //  learners foreach { l =>
  //    val m = l.build(pool)
  //    println(s"$l : ${m.accuracy(ts)}    L: ${m.asInstanceOf[ELMModel].L}")
  //  }
  val n = 11
  val a = learners.map(l => l.build(pool.take(n))).toArray
  pool.drop(n).foreach { x =>
    val accs = learners.zipWithIndex map { case (l, i) =>
      a(i) = l.update(a(i))(x)
      a(i).accuracy(ts)
    }
    println(accs.mkString(" "))
  }
}

object interawfELMTest extends App {
  //  val patts0 = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci")("gas-drift").right.get.take(1000000))
  //  val patts0 = new Random(0).shuffle(Datasets.arff(true)("/home/davi/wcs/ucipp/uci/banana.arff").right.get.take(200000))
  //    val patts0 = new Random(0).shuffle(Datasets.arff(true)("/home/davi/wcs/ucipp/uci/iris.arff").right.get.take(200000))
  val patts0 = new Random(10).shuffle(Datasets.arff(true)("/home/davi/wcs/ucipp/uci/abalone-11class.arff").right.get.take(200000))
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilter(patts0, filter)

  val n = patts.length / 2
  val tr = patts.take(n)
  val ts = patts.drop(n)

  //interaELMNoEM(n)
  //KNN(5,"eucl",patts)
  val tt = patts.head.nclasses
  val l = EMELM(50)
  Tempo.start
  var m = l.build(tr.take(99800))
  println(s"${m.accuracy(ts)}")
  tr.drop(tt).zipWithIndex foreach { case (x, i) =>
    //    m = l.update(m)(x)
    m = OSELM((tt + i + 1) / 2).build(tr.take(tt + i + 1))
    println(s"${m.accuracy(ts)}")
  }
  Tempo.print_stop
}