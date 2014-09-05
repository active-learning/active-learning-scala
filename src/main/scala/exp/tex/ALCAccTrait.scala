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

package exp.tex

import app.db.entities.Dataset
import exp.result.Res
import ml.classifiers.Learner
import util.{Stat, StatTests}

import scala.collection.mutable

trait ALCAccTrait extends Res {
  val desc: String
  lazy val medida = "ALCDaAcc"
  val readOnly = true
  val mat = mutable.LinkedHashMap[String, Seq[(Double, Double)]]()
  var sts = mutable.LinkedHashSet[String]()
  val learners: Seq[Learner]

  def core(db: Dataset, sid: Int, Q: Int, st: String, le: String, lid: Int) = {
    val seq = mat.getOrElse(db.toString, Seq()) // seq of ALCavgs (one for each strat)
    db.exec(s"select v from res where m=$mid and s=$sid and l=$lid group by r,f order by r,f") match {
      case None | Some(mutable.Queue()) =>
        println(s"Skipping $db/$st")
        if (mat.exists(_._1 == db.toString)) {
          mat.update(db.toString, seq :+(-1d, -1d))
          sts += s"$st${le.split(" ").head.dropRight(1)}"
        }
        false
      case Some(q) =>
        val ALCs = q.map(_.head).toVector
        val (m, s) = Stat.media_desvioPadrao(ALCs)
        mat.update(db.toString, seq :+(m, s))
        sts += s"$st${le.split(" ").head.dropRight(1)}"
        true
    }
  }

  def end() = {
    val abr = 5
    val matClean = mat.filterNot(_._2.contains((-1d, -1d)))
    println(matClean)
    val stsClean = sts.zip(mat).filter { case (st, (da, li)) => matClean.contains(da)} map (_._1)
    val mats = matClean.toSeq.sortBy(_._1).map(x => x._1.take(abr) + " " + x._1.drop(abr).takeRight(2) -> x._2)

    val matm = mats.map(x => x._1.take(abr) + " " + x._1.drop(abr).takeRight(2) -> x._2.map(_._1))

    println("Completos:")
    println(datasetNames.diff(excluidos).mkString(","))

    println("Incompletos:")
    println(excluidos.mkString(","))

    println(mats)
    println("")
    println("extensive ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    println("")
    mats.grouped(50) foreach { g =>
      StatTests.extensiveTable2(g, stsClean.toVector, "tabalcacc", medida)
      println("")
    }

    println("")
    println("1vs1 -----------------------------------------------------------------------")
    println("")
    StatTests.pairTable(StatTests.friedmanNemenyi(matm, stsClean.toVector), "tabalcacc", medida)
  }
}