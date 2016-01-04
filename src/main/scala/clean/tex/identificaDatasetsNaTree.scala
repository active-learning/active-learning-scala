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

object identificaDatasetsNaTree extends Exp with Lock {
  val arguments = superArguments
  val context = "datasetsdesc"
  val m = mutable.Map[String, Ds]()
  val ignoreNotDone = false
  override val folds = 2
  override val runs = 1
  override val readOnly = true
  run()

  def strats(pool: Seq[Pattern], seed: Int) = List(RandomSampling(pool))

  def isAlreadyDone(ds: Ds) = false

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
  }

  def datasetFinished(ds: Ds) {
    acquire()
    m += renomeia(ds.dataset) -> ds
    release()
  }

  lazy val descriptionNames = Seq( """\pbox{20cm}{\#exemplos\\($|\mathcal{U}|$)}""", """\pbox{20cm}{\#classes\\($|Y|$)}""", "\\#atributos", "\\#nominais", "\\%majoritária", "\\%minoritária") //, """\pbox{20cm}{entropia da \\distr. de classes}""")

  lazy val header = " & \\rotatebox{0}{$|\\mathcal{U}|$} & \\rotatebox{0}{$|Y|$} & \\rotatebox{0}{atributos} & \\rotatebox{0}{nominais} & \\rotatebox{0}{\\makecell{majoritária\\\\(\\%)}} & \\rotatebox{0}{\\makecell{minoritária\\\\(\\%)}}\\\\ \\hline"

  def end(res: Map[String, Boolean]) {
    val todas = m.toList.sortBy(_._1).zipWithIndex filter { case ((d, ds), i) =>
      ds.open()
      val r = ds.nominalDistinctCount.min > 1
      ds.close
      r
    }
    println(todas.map { case ((d, ds), i) => (i+1) + "-" + ds }.mkString(", "))
  }
}
