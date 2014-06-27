package exp.other

import ml.classifiers.Learner
import util.Datasets

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
//object ComparingClassifiers {
//  val passive_accs = false
//  val datasets0 = "tae,wine,statlog-heart,flare,molecular-promoters,leukemia-haslinger,balance-scale,pima,car,breast-cancer-wisconsin-diagnostic,wine-quality-red,connectionist-mines-vs-rocks,cmc,vowel,monk1,breast-tissue,ionosphere,subject,australian,newthyroid,colon32,hayes-roth,bodies,vehicle,accute-inflammations,iris,yeast-4classes,tic-tac-toe".split(",")
//  val datasets = datasets0.sorted
//  //class imbalance can be calculated by \textit{normalized entropy} \cite{lewin2004quantitative}
//  val strs = for (dataset <- datasets) yield {
//    val arff = dataset_path + dataset + ".arff"
//    val d = Datasets.arff(bina = false)(arff) match {
//      case Right((x,_,_)) => x
//      case Left(str) => println(str); sys.exit(0)
//    }
//    val tot = d.head.nattributes
//    val nominais = (for (i <- 0 until tot) yield d.head.dataset.attribute(i).isNominal) count (_ == true)
//    val numerics = tot - nominais
//    val nclasses = d.head.numClasses
//    val n = d.length
//    val counts = (0 until nclasses) map (c => (0 until n) count (d(_).classValue == c))
//    val p = (counts map (_ / n.toDouble)).toArray
//    val maj = p.max
//
//    def acc(le: Learner) = {
//      (0 until 10).map {
//        l =>
//          val folds = Datasets.kfoldCVold(d, 1).right.get
//          val res = (0 until 10).par map {
//            k =>
//              val model = le.build(folds(k).train)
//              (model.hits(folds(k).test), folds(k).test.length)
//          }
//          res.map(_._1).sum.toDouble / res.map(_._2).sum
//      }.sum / 10d
//    }
//
//    //    lazy val accc45 = acc(C45(3))
//    //    lazy val accht = acc(HT())
//    //    lazy val accnb = acc(NB())
//    //    lazy val acc5nn = acc(KNN(5, "eucl"))
//
//    //      println("   " + abbrev(dataset) + " & " + n + " & " + numerics + " & " + nominais + " & " + nclasses + " & " + "%.2f".format(1 - normalized_entropy(p)) + " \\\\ ")
//    //    if (passive_accs) "   " + abbrev(dataset) + " & " + n + " & " + numerics + " & " + nominais + " & " + nclasses + " & " + "%.2f".format(accc45) + " & " + "%.2f".format(accnb) + " & " + "%.2f".format(accht) + " & " + "%.2f".format(acc5nn) + " & " + "%.2f".format(maj) + " \\\\ "
//    "   " + abbrev(dataset) + " & " + n + " & " + numerics + " & " + nominais + " & " + nclasses + " & " + "%.2f".format(maj) + " \\\\ "
//  }
//  println( """
//\begin{table}[h]
//\caption{Dataset details. Last column indicates the proportion of the majoritary class.""" + (if (passive_accs) """ Passive accuracies are also provided.}""" else """}""") +
//    """\begin{center}
//\begin{tabular}{|l|r|r|r|r|r|r|r|r|r|}
//   \hline
//   \multicolumn{1}{|c|}{Dataset}
//   & \multicolumn{1}{|c|}{\#Instances}
//   & \multicolumn{1}{|c|}{\#Numeric}
//   & \multicolumn{1}{|c|}{\#Nominal}
//   & \multicolumn{1}{|c|}{\#Classes}""" +
//    (if (passive_accs) """& \multicolumn{1}{|c|}{C4.5}
//   & \multicolumn{1}{|c|}{NB}
//   & \multicolumn{1}{|c|}{HT}
//   & \multicolumn{1}{|c|}{5-NN}"""
//    else """ """) +
//    """& \multicolumn{1}{|c|}{Imb.} \\
//   \hline
//    """)
//
//  println(strs.mkString("\n") + """
//\hline
//\end{tabular}
//\label{details}
//\end{center}
//\end{table}
//                                """)
//
//}
