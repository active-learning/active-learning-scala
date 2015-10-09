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
import clean.lib._
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
      val (pool, fpool) = pool0 -> fpool0 //redux(pool0, ds) -> redux(fpool0, ds)
      val qs = maxQueries(ds)
      ds.log(s"${pool.size} amostrados")
      stratsemLearnerExterno(pool) foreach (strat => gravaTempo(ds, poolSize, strat, qs, fold))
     stratcomLearnerExterno(NB(), fpool) foreach (strat => gravaTempo(ds, poolSize, strat, qs, fold))
      //      stratcomLearnerExterno(CIELM(System.currentTimeMillis().toInt), fpool) foreach (strat => gravaTempo(ds, poolSize, strat, qs, run, fold))
     stratcomLearnerExterno(NB(), fpool) foreach (strat => gravaTempo(ds, poolSize, strat, qs, fold))
      Seq(NB(), KNNBatchb(5, "eucl", ds.patterns, weighted = true), SVMLibDegree1(System.currentTimeMillis().toInt)).foreach { learner =>
         stratsComLearnerExterno_FilterFree(pool, learner) foreach (strat => gravaTempo(ds, poolSize, strat, qs, fold))
         stratsComLearnerExterno_FilterDependent(fpool, learner) foreach (strat => gravaTempo(ds, poolSize, strat, qs, fold))
      }
      //      stratsSGmajJS(fpool, IELM(System.currentTimeMillis().toInt)) foreach (strat => gravaTempo(ds, poolSize, strat, qs, run, fold))
      //      stratsSGmajJS(fpool, ninteraELM(System.currentTimeMillis().toInt)) foreach (strat => gravaTempo(ds, poolSize, strat, qs, run, fold))
      //      Seq(NB(), KNNBatch(5, "eucl", ds.patterns, weighted = true), SVMLib(System.currentTimeMillis().toInt)).foreach { learner =>
      //         stratsSGmajJS(pool, learner) foreach (strat => gravaTempo(ds, poolSize, strat, qs, run, fold))
      //      }
   }

   def gravaTempo(ds: Ds, poolSize: Int, strat: Strategy, qs0: Int, f: Int) = {
      val qs = math.max(qs0, ds.nclasses + 3)
      lazy val q = strat.queries
      lazy val elapsedi = Tempo.time {
         q.take(ds.nclasses + 1)
      }
      lazy val elapsed = poolSize * Tempo.time {
         q.take(qs)
      } / qs

      //warming time 1000 + #queries
      //avg querying time 10000 + #queries
      //0.1*pool time 50000 + #queries
      (0 until Global.runs).foreach { rr =>
         ds.read(s"select id from p where s=${strat.id} and l=${strat.learner.id} and f=$f and r=$rr") match {
            case List(Vector(pid)) =>
               val prev = ds.read(s"select count(0) from r where p=$pid and m=${1000 + qs0}").head.head.toInt
               if (prev == 0) {
                  val inserts = List(s"insert into r values (${1000 + qs0}, $pid, $elapsedi)"
                     , s"insert into r values (${10000 + qs0}, $pid, ${elapsed / poolSize})"
                     , s"insert into r values (${50000 + qs0}, $pid, ${0.1 * elapsed})")
                  acquire()
                  sqls.enqueue(inserts: _*)
                  release()
               }
            case x => ds.log(s"Pool ainda não criado para essa strat/learner/r/f.\n$x")
         }
      }
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
      //      val qs = maxQueries(ds)
      //      //é tudo ou nada; ou gravou tempo do dataset inteiro, ou não gravou nada.
      //      val prev = ds.read(s"select count(0) from r,p where m=${1000 + qs} and p=id and s in (15,16)").head.head.toInt
      //      prev != 0
      false
   }

   def end(res: Map[String, Boolean]): Unit = {
   }
}
