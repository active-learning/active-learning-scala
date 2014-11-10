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
package clean.meta

import java.io.FileWriter

import clean.res.ALCgmeans
import clean._
import util.{StatTests, Stat}

object arfftie extends AppWithUsage with StratsTrait with LearnerTrait {
  val context = "metaAttsApp"
  val arguments = superArguments
  run()

  def r(x: Double) = (x * 100).round / 100d

  override def run() = {
    super.run()
    Seq(ALCgmeans(), null, null).dropRight(2) foreach { measure =>
      val strats = allStrats()
      val ss = strats.map(_.abr).toVector

      val datasetLearnerAndBoth = for {
        dataset <- datasets.toList
        l <- allLearners()
      } yield {
        val ds = Ds(dataset)
        ds.open()
        try {
          val vs = for {
            t <- Seq(0, 1)
            r <- 0 until runs
            f <- 0 until folds
          } yield {
            val poolStr = (100 * r + f).toString
            val sres = for {
              s <- strats
            } yield {
              val le = if (s.id >= 17 && s.id <= 21) s.learner else l
              if (!ds.isMeasureComplete(measure, s.id, le.id)) throw new Exception()
              if (measure.id == 0) throw new Exception()
              ds.getMeasure(measure, s, le, r, f) match {
                case Some(v) => v
                case None => ds.quit(s"No measure for ${(measure, s, le, r, f)}!")
              }
            }
            poolStr -> sres
          }
          val pred = (ds.metaAtts.map(_.toString), l.abr)
          Some((ds.dataset + l.toString.take(3), pred) -> StatTests.winners(vs, ss), (ds.dataset + l.toString.take(3), pred) -> StatTests.losers(vs, ss))
        } catch {
          case _: Throwable => None
        } finally {
          ds.close()
        }
      }

      val (datasetLearnerAndWinners, datasetLearnerAndLosers) = datasetLearnerAndBoth.flatten.unzip
      val grpdw = ss map (w => w -> datasetLearnerAndWinners.filter(_._2.contains(w)).map { case ((darner, (pred, le)), ws) =>
        pred.mkString(",") + s",$le,$w"
      })

      //cria ARFF
      val labels = ss.distinct.sorted
      println(labels)
      grpdw foreach { case (w, lines) =>
        val header = List("@relation data") ++ (0 until lines.head.toList.count(_ == ",")).map(i => s"@attribute a$i numeric") ++ List("@attribute learner {" + allLearners().map(_.abr).mkString(",") + "}", "@attribute class {" + labels.mkString(",") + "}", "@data")
        val pronto = header ++ lines
        pronto foreach println
        println(s"")
        val fw = new FileWriter(s"/home/davi/wcs/ucipp/uci/metatie-$w.arff")
        pronto foreach (x => fw.write(s"$x\n"))
        fw.close()
      }
      println(s"")
    }
  }
}
