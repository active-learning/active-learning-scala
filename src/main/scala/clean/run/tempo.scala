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

import al.strategies.Strategy
import clean._
import ml.Pattern
import ml.classifiers._
import util.Tempo
import weka.filters.Filter

import scala.collection.mutable

object tempo extends Exp with LearnerTrait with StratsTrait with Lock {
   val context = "tempoApp"
   val arguments = superArguments
   val ignoreNotDone = false
   val sqls = mutable.Queue[String]()
   override lazy val runs = 1
   run()

   /**
    * Take 5% of dataset as pool, or 50, whatever is higher.
    * @param s
    * @param ds
    * @return
    */
   def redux(s: Seq[Pattern], ds: Ds) = s.groupBy(_.label).flatMap(_._2.take(math.max((ds.n / 10) / ds.nclasses, 50 / ds.nclasses))).toList

   def op(ds: Ds, pool0: Seq[Pattern], testSet: Seq[Pattern], fpool0: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      val poolSize = ds.expectedPoolSizes(5).min
      val (pool, fpool) = redux(pool0, ds) -> redux(fpool0, ds)
      val qs = maxQueries(ds)
      val prev = ds.read(s"select count(0) from r where m=${1000 + qs}").head.head.toInt
      if (prev == 0) {
         ds.log(s"${pool.size} amostrados")
         stratsemLearnerExterno(pool) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         stratcomLearnerExterno(IELM(System.currentTimeMillis().toInt), fpool) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         //      stratcomLearnerExterno(CIELM(System.currentTimeMillis().toInt), fpool) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         stratcomLearnerExterno(ninteraELM(System.currentTimeMillis().toInt), fpool) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         Seq(NB(), KNNBatch(5, "eucl", ds.patterns, weighted = true), SVMLib(System.currentTimeMillis().toInt)).foreach { learner =>
            stratsComLearnerExterno_FilterFree(pool, learner) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
            stratsComLearnerExterno_FilterDependent(fpool, learner) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         }
      } else {
         //completa com sgs que nao tinha antes
         ds.log(s"${pool.size} amostradoss")
         stratsSGmajJS(fpool, IELM(System.currentTimeMillis().toInt)) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         stratsSGmajJS(fpool, ninteraELM(System.currentTimeMillis().toInt)) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         Seq(NB(), KNNBatch(5, "eucl", ds.patterns, weighted = true), SVMLib(System.currentTimeMillis().toInt)).foreach { learner =>
            stratsSGmajJS(pool, learner) foreach (strat => gravaTempo(poolSize, strat, qs, run, fold))
         }
      }
   }

   def gravaTempo(poolSize: Int, strat: Strategy, qs: Int, r: Int, f: Int) = {
      val elapsedi = Tempo.time {
         strat.queries.take(1)
      }
      val elapsed = poolSize * Tempo.time {
         strat.queries.take(qs)
      } / qs

      //warming time 1000 + #queries
      //avg querying time 10000 + #queries
      //0.1*pool time 50000 + #queries
      val inserts = (0 until Global.runs).flatMap { rr =>
         List(s"insert into r select ${1000 + qs}, id, $elapsedi from p where s=${strat.id} and l=${strat.learner.id} and r=$rr and f=$f"
            , s"insert into r select ${10000 + qs}, id, ${elapsed / poolSize} from p where s=${strat.id} and l=${strat.learner.id} and r=$rr and f=$f"
            , s"insert into r select ${50000 + qs}, id, ${0.1 * elapsed} from p where s=${strat.id} and l=${strat.learner.id} and r=$rr and f=$f")
      }.toList
      acquire()
      sqls.enqueue(inserts: _*)
      release()
      //            println(s"$sql")
   }

   def datasetFinished(ds: Ds) {
      if (sqls.nonEmpty) {
         ds.batchWrite(sqls.toList)

         println(s"")
         println(s"")
         sqls.toList foreach println
         println(s"")

         sqls.clear()
      }
   }

   def isAlreadyDone(ds: Ds) = {
      val contapid = ds.read(s"select count(0) from r,p where m=1020 and p=id and s in (15,16)").head.head.toInt
      if (contapid == 0) ds.quit("Pool ainda não criado para essa strat/learner/r/f.")
      //é tudo ou nada; ou gravou tempo do dataset inteiro, ou não gravou nada.
      val qs = maxQueries(ds)
      val prev = ds.read(s"select count(0) from r,p where m=${1000 + qs} and p=id and s in (15,16)").head.head.toInt
      prev != 0
   }

   def end(res: Map[String, Boolean]): Unit = {
   }
}
