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
import util.Tempo
import weka.filters.Filter

import scala.collection.mutable

object acvTempo extends Exp with LearnerTrait with StratsTrait {
  val context = "acvApp"
  val arguments = superArguments :+ "leas(unused)" :+ "versao"
  val ignoreNotDone = false
  run()

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
    ds.log(s"Iniciando tempo para pool $run.$fold ...", 30)
    val (_, t) = Tempo.timev {
      learnersFpool(learnerSeed) foreach { flearner =>
        stratsForTempo(pool, fpool, flearner) foreach { strat =>
          ds.log(s"$flearner $strat ...")
          strat.queries.take(50).toList
        }
      }
    }
    println(s"$ds $run.$fold $t")
  }

  def datasetFinished(ds: Ds) = {
  }

  def isAlreadyDone(ds: Ds) = false

  def end(res: Map[String, Boolean]): Unit = {
  }
}
