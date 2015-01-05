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

import java.io.{PrintWriter, FileWriter}

import al.strategies.RandomSampling
import clean.{Ds, Exp, Lock}
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
      m += ds.dataset.take(20) -> ds.description._1.map(_.toString) //++ Seq("%5.2f".format(ds.description._2)))
      release()
   }

   def tabela(tableName: String, caption: String, core: Seq[(String, Seq[String])]) = {
      val cols = descriptionNames.size - 1
      """
\begin{table}[h]
\caption{""" + caption + """}
\begin{center}
\begin{tabular}{l|""" + Seq.fill(cols)("r").mkString(" ") + "}\n & " + descriptionNames.dropRight(1).map(x => "\\rotatebox{0}{" + x + "}").mkString(" & ") + """\\ \hline """ +
         core.zipWithIndex.map { case (x, i) => (x._1 +: x._2).mkString(" & ") + "\\\\" + (if (i % 3 - 1 == 2) "\\hline" else "")}.mkString("\n") +
         """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}"""
   }

   def end(res: Map[String, Boolean]) {
      val fw = new PrintWriter("/home/davi/wcs/tese/dataset-tables.tex", "ISO-8859-1")
      val todas = m.toList.sortBy(_._1)
      fw.write( """\definecolor{darkgreen}{rgb}{0.0, 0.4, 0.0}""")
      fw.write(tabela("tab:datasetsa", "Características das bases de dados (1/3).", todas.take(33)))
      fw.write(tabela("tab:datasetsb", "Características das bases de dados (2/3).", todas.drop(33).take(33)))
      fw.write(tabela("tab:datasetsc", "Características das bases de dados (3/3).", todas.drop(66)))

      val maisDesbalanceadas = m.filter(x => x._2(4).toDouble > 4 * x._2(5).toDouble).toList.sortBy(x => x._2(4).toDouble / x._2(5).toDouble).reverse
      fw.write(tabela("tab:imb", "Bases de dados mais desbalanceadas.", maisDesbalanceadas))

      val maisAtributos = m.filter(x => x._2(2).toDouble > 50).toList.sortBy(x => x._2(2).toDouble).reverse
      fw.write(tabela("tab:x", "Bases de dados com mais atributos.", maisAtributos))

      val maisClasses = m.filter(x => x._2(1).toDouble > 5).toList.sortBy(x => x._2(1).toDouble).reverse
      fw.write(tabela("tab:y", "Bases de dados com mais classes.", maisClasses))

      val maisExemplos = m.filter(x => x._2(0).toDouble > 1000).toList.sortBy(x => x._2(0).toDouble).reverse
      fw.write(tabela("tab:n", "Bases de dados com mais exemplos.", maisExemplos))
      fw.close()
   }
}
