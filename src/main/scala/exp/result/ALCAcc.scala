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

package exp.result

import app.ArgParser
import app.db.entities.Dataset
import ml.classifiers.Learner

object ALCAcc extends Res {
  val desc = s"Version ${ArgParser.version} \nCalcula ALCs e coloca na tabela 'res'."
  lazy val medida = "ALCDaAcc"
  val readOnly = false
  val learners = Seq(learner(-1, Seq()))

  def core(db: Dataset, sid: Int, Q: Int, st: String, le: String, lid: Int) = {
    var r = false
    val hits = db.exec(s"select sum(value),run,fold from hit where strategyid=$sid and learnerid=$lid and pred=expe and position<$Q group by run,fold order by run,fold").get
    val tries = db.exec(s"select sum(value),run,fold from hit where strategyid=$sid and learnerid=$lid and position<$Q group by run,fold order by run,fold").get
    val expectedTries = db.n * runs * (Q - db.nclasses)
    if (expectedTries < tries.map(_.head).sum) db.safeQuit(s"Inconsistency: $expectedTries expected tries for $st/$le/$db is lesser than ${tries.map(_.head).sum} tries found (tries = number os instances in the test set * (Q - |Y|) * runs).")
    else if (expectedTries > tries.map(_.head).sum) println(s"Skipping: $expectedTries expected for $st/$le/$db; ${tries.map(_.head).sum} found.")
    else {
      if (hits.zip(tries).exists(x => x._1.head > x._2.head)) db.safeQuit(s"Inconsistency: one of the $hits expected hits for $st/$le/$db is greater than one of the $tries tries found (tries = number os instances in the test set * (Q - |Y|) * runs).")
      hits.zip(tries).foreach { case (h, t) =>
        val alc = h.head / t.head
        val r = h(1)
        val f = h(2)
        db.exec(s"insert into res values ($mid, $sid, $lid, $r, $f, $alc)")
        //        println(s"insert into res values (2, $sid, $lid, $r, $f, $alc)")
      }
      r = true
    }
    r
  }

  run()

  def end() {}
}

//atQ  iris: Queue(0.9333333333333333, 0.8, 0.896551724137931, 0.9655172413793104, 0.9310344827586207, 0.9333333333333333, 0.8333333333333334, 0.9310344827586207, 0.7931034482758621, 0.896551724137931, 0.8666666666666667, 0.8666666666666667, 0.9655172413793104, 1.0, 0.896551724137931, 0.8666666666666667, 0.9666666666666667, 0.7586206896551724, 0.8275862068965517, 0.8620689655172413, 0.9666666666666667, 0.9, 0.9310344827586207, 0.9655172413793104, 0.9310344827586207)
//ALC  iris: Queue(0.9333333333333333, 0.8, 0.887147354231975, 0.9623824451410659, 0.9028213166144201, 0.8727272727272727, 0.7484848484848485, 0.9592476489028213, 0.7523510971786834, 0.771159874608105, 0.8757575757575757, 0.7636363636363637, 0.9216300940438872, 0.6, 0.877742946708464, 0.8333333333333334, 0.9393939393939394, 0.7335423197492164, 0.8840125391849529, 0.8463949843260188, 0.896969696969697, 0.52, 0.8526645768025078, 0.9278996865203761, 0.8934169278996865)
