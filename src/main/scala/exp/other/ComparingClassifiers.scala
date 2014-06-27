package exp.other

import app.ArgParser
import app.db.Dataset
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

//  val accs = mutable.Queue[Seq[Double]]()
//  val ts = mutable.Queue[Seq[Double]]()

  def runCore(db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: => Seq[Pattern]) {
    val learners = Seq(IELM(pool.size), EIELM(pool.size), CIELM(pool.size), interaELM(pool.size / 3), OSELM(math.sqrt(pool.size).toInt), HT(), NB(), KNN(5, "eucl"))
//    val accs_ts = learners fore { learner =>
//      val (m, t) = Tempo.timev(learner.build(pool))
//      val acc = m.accuracy(testSet)
//      (acc, t)
//    }
//    accs.enqueue(accs_ts.map(_._1))
//    ts.enqueue(accs_ts.map(_._2))

  }

  run

  //    val tot = d.head.nattributes
  //    val nominais = (for (i <- 0 until tot) yield d.head.dataset.attribute(i).isNominal) count (_ == true)
  //    val numerics = tot - nominais
  //    val nclasses = d.head.numClasses
  //    val n = d.length
  //    val counts = (0 until nclasses) map (c => (0 until n) count (d(_).classValue == c))
  //    val p = (counts map (_ / n.toDouble)).toArray
  //    val maj = p.max

  //    lazy val accc45 = acc(C45(3))
  //    lazy val accht = acc(HT())
  //    lazy val accnb = acc(NB())
  //    lazy val acc5nn = acc(KNN(5, "eucl"))

  //      println("   " + abbrev(dataset) + " & " + n + " & " + numerics + " & " + nominais + " & " + nclasses + " & " + "%.2f".format(1 - normalized_entropy(p)) + " \\\\ ")
  //    if (passive_accs) "   " + abbrev(dataset) + " & " + n + " & " + numerics + " & " + nominais + " & " + nclasses + " & " + "%.2f".format(accc45) + " & " + "%.2f".format(accnb) + " & " + "%.2f".format(accht) + " & " + "%.2f".format(acc5nn) + " & " + "%.2f".format(maj) + " \\\\ "
  //    "   " + abbrev(dataset) + " & " + n + " & " + numerics + " & " + nominais + " & " + nclasses + " & " + "%.2f".format(maj) + " \\\\ "

  println( """
\begin{table}[h]
\caption{Dataset details. Last column indicates the proportion of the majoritary class. Passive accuracies are also provided.}""" +
    """\begin{center}
\begin{tabular}{|l|r|r|r|r|r|r|r|r|r|}
   \hline
   \multicolumn{1}{|c|}{Dataset}
   & \multicolumn{1}{|c|}{\#Instances}
   & \multicolumn{1}{|c|}{\#Numeric}
   & \multicolumn{1}{|c|}{\#Nominal}
   & \multicolumn{1}{|c|}{\#Classes}""" +
    """& \multicolumn{1}{|c|}{C4.5}
   & \multicolumn{1}{|c|}{NB}
   & \multicolumn{1}{|c|}{HT}
   & \multicolumn{1}{|c|}{5-NN}"""
  )

  //  println(strs.mkString("\n") + """
  //\hline
  //\end{tabular}
  //\label{details}
  //\end{center}
  //\end{table}
  //                                """)

}
