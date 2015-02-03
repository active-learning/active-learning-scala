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

package clean.tex

import java.io.PrintWriter

import al.strategies.RandomSampling
import clean.lib.{Ds, Exp, Lock}
import ml.Pattern
import weka.filters.Filter

import scala.collection.mutable

object datasetsdesc extends Exp with Lock {
   val arguments = superArguments
   val context = "datasetsdesc"
   val m = mutable.Map[String, List[String]]()
   val ignoreNotDone = false
   override val readOnly = true
   run()

   def strats(pool: Seq[Pattern], seed: Int) = List(RandomSampling(pool))

   def isAlreadyDone(ds: Ds) = false

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
   }

   def datasetFinished(ds: Ds) {
      acquire()
      m += renomeia(ds) -> (ds.description._1.map(_.toString) ++ ds.description._2.dropRight(1).map(x => "%5.1f".format(x)))
      release()
   }

   def tabela(tableName: String, caption: String, core: Seq[(String, Seq[String])]) = {
      val cols = descriptionNames.size - 1
      """
\begin{table}[h]
\caption{""" + caption + """}
\begin{center}
\begin{tabular}{l|""" + Seq.fill(cols)("r").mkString(" ") + "}\n & " + descriptionNames.dropRight(1).map(x => "\\rotatebox{0}{" + x + "}").mkString(" & ") + """\\ \hline """ +
         core.zipWithIndex.map { case (x, i) => (x._1 +: x._2).mkString(" & ") + "\\\\" + (if (i % 3 == 2) "\\hline" else "")}.mkString("\n") +
         """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}"""
   }

   def end(res: Map[String, Boolean]) {
      val todas = m.toList.sortBy(_._1).zipWithIndex.map { case ((d, l), i) =>
         ((i + 1) + "-" + d) -> l
      }
      println(todas.size + " " + datasets.size)
      val fw2 = new PrintWriter("/home/davi/wcs/tese/dataset-tables.tex", "ISO-8859-1")
      fw2.write(tabela("tab:datasetsa", "Características das bases de dados (1-33).", todas.take(33)))
      fw2.write(tabela("tab:datasetsb", "Características das bases de dados (34-66).", todas.drop(33).take(33)))
      fw2.write(tabela("tab:datasetsc", "Características das bases de dados (67-94).", todas.drop(66)))
      fw2.close()

      val fw = new PrintWriter("/home/davi/wcs/tese/dataset-tables-reduxes.tex", "ISO-8859-1")
      val maisDesbalanceadas = todas.filter(x => x._2(4).toDouble > 4 * x._2(5).toDouble).toList.sortBy(x => x._2(4).toDouble / x._2(5).toDouble).reverse
      fw.write(tabela("tab:imb", "Bases de dados mais desbalanceadas.", maisDesbalanceadas))

      val maisAtributos = todas.filter(x => x._2(2).toDouble > 50).toList.sortBy(x => x._2(2).toDouble).reverse
      fw.write(tabela("tab:x", "Bases de dados com mais atributos.", maisAtributos))

      val maisClasses = todas.filter(x => x._2(1).toDouble > 5).toList.sortBy(x => x._2(1).toDouble).reverse
      fw.write(tabela("tab:y", "Bases de dados com mais classes.", maisClasses))

      val maisExemplos = todas.filter(x => x._2(0).toDouble > 1000).toList.sortBy(x => x._2(0).toDouble).reverse
      fw.write(tabela("tab:n", "Bases de dados com mais exemplos.", maisExemplos))

      val menosExemplos = todas.filter(x => x._2(0).toDouble < 400).toList.sortBy(x => x._2(0).toDouble)
      fw.write(tabela("tab:nm", "Bases de dados com menos exemplos.", menosExemplos))
      fw.close()
   }
}
