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

package clean

import java.util.Calendar

import al.strategies.{ClusterBased, RandomSampling}
import util.Datasets

import scala.io.Source
import scala.util.Random

object Q extends AppWithUsage {
  val arguments = List("file-with-dataset-names")
  init()

  val datasets = Source.fromFile(args(0)).getLines().filter(_.length > 2)

  datasets foreach { dataset =>
    def log(msg: String): Unit = {
      println(s"${Calendar.getInstance().getTime}\n $dataset : $msg")
    }
    val ds = Ds("/home/davi/wcs/ucipp/uci")(dataset)
    log(s"Processing ${ds.n} instances ...")
    (0 until runs par) foreach { run =>
      val shuffled = new Random(run).shuffle(ds.patterns)
      Datasets.kfoldCV(shuffled, k = folds, parallel = true) { (tr, ts, fold, minSize) =>
        log(s"Pool $run.$fold (${tr.size} instances) ...")

        //Ordena pool e faz versão filtrada.
        val pool = new Random(fold).shuffle(tr.sortBy(_.id))
        val filteredPool = {
          val binaf = Datasets.binarizeFilter(tr)
          val binarizedTr = Datasets.applyFilter(binaf)(tr)
          val zscof = Datasets.zscoreFilter(binarizedTr)
          val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
          new Random(fold).shuffle(filteredTr.sortBy(_.id))
        }
        //        if (pool.zip(filteredPool).forall(x => x._1.id == x._2.id)) log("Ids foram mantidos após filtro.")
        //        else throw new Error("Ids inconsistentes!")

        //Grava rnd e clu queries.
        List(RandomSampling(pool), ClusterBased(pool)) foreach { strat =>
          log(s"$strat ...")
          ds.write(s"INSERT OR IGNORE INTO p VALUES (NULL, ${strat.id}, 0, $run, $fold)")
          val poolId = ds.read(s"SELECT id FROM p WHERE s=${strat.id} and l=0 and r=$run and f=$fold").head.head.toInt
          val gravou = ds.read(s"SELECT COUNT(1) FROM q WHERE p=$poolId").head.head match {
            case 0 => false
            case qs => if (qs != pool.size) ds.quit(s"$qs previous queries should be ${pool.size}") else true
          }
          if (gravou) log(s"Queries do pool $run.$fold já estavam gravadas para $strat.")
          else {
            val sqls = strat.queries.zipWithIndex map { case (q, t) => s"INSERT INTO q VALUES ($poolId, $t, ${q.id})"}
            ds.batchWrite(sqls.toList)
          }
          log(s"$strat ok.")
        }
      }
    }
    ds.close()
  }
}