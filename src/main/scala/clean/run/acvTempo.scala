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

package clean.run

import clean.lib._
import ml.Pattern
import ml.classifiers.RoF
import util.Tempo
import weka.filters.Filter

import scala.collection.mutable

object acvTempo extends Exp with LearnerTrait with StratsTrait {
  val context = "acvApp"
  val arguments = superArguments :+ "leas(unused)" :+ "versao(unused)"
  val ignoreNotDone = false
  var t = 0d
  run()

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
    ds.log(s"Iniciando tempo para pool $run.$fold ...", 30)
    val flearner = RoF(learnerSeed, 2)
    stratsForTempo(pool, fpool, flearner) foreach { strat =>
      ds.log(s"$flearner $strat ...")
      strat.queries.take(50).toList
    }
  }

  def isAlreadyDone(ds: Ds) = {
    t = Tempo.now
    false
  }

  def datasetFinished(ds: Ds) = {
    println(s"tempo $ds ${(Tempo.now - t) / 1000d}")
  }

  def end(res: Map[String, Boolean]): Unit = {
  }
}
