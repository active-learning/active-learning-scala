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
      m += ds.dataset.take(20) -> (ds.description._1.map(_.toString) ++ Seq("%5.2f".format(ds.description._2)))
      release()
   }

   def tabela(tableName: String, caption: String, core: Seq[(String, Seq[String])]): Unit = {
      val cols = descriptionNames.size
      println( """\definecolor{darkgreen}{rgb}{0.0, 0.4, 0.0}
\begin{table}[h]
\caption{""" + caption + """}
\begin{center}
\begin{tabular}{l""" + Seq.fill(cols)("p{1cm}").mkString("|") + "}\n & " + descriptionNames.map(x => "\\rotatebox{60}{" + x + "}").mkString(" & ") + """\\ \hline """)
      println(core.map(x => (x._1 +: x._2).mkString(" & ")).mkString( """\\ \hline """ + "\n"))
      println( """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}""")
   }

   def end(res: Map[String, Boolean]): Unit = {
      val caption = "Características das bases de dados."

      val core = m.toList.sortBy(_._1)
      tabela("tab:datasetsa", caption, core.take(33))
      tabela("tab:datasetsb", caption, core.drop(33).take(33))
      tabela("tab:datasetsc", caption, core.drop(66))
   }
}
