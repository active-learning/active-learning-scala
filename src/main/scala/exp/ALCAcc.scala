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

package exp

import app.ArgParser
import app.db.entities.{AppFile, Dataset}

object ALCAcc extends Res {
  val desc = s"Version ${ArgParser.version} \nCalcula ALCs e coloca na tabela 'res'."

  def core(db: Dataset) {
    strats foreach { st =>
      val Q = db.Q
      val s = sid(st)
      val hits = db.exec(s"select sum(value) from hit where strategyid=$s and learnerid=$lid and pred=expe and position<$Q group by run,fold").get.map(_.head)
      val tries = db.exec(s"select sum(value) from hit where strategyid=$s and learnerid=$lid and position<$Q group by run,fold").get.map(_.head)
      val expectedTries = db.n * runs * (Q - db.nclasses)
      if (expectedTries != tries.sum) db.safeQuit(s"$expectedTries expected tries is different from ${tries.sum} tries found (tries = number os instances in the test set * (Q - |Y|) * runs).")
      if (hits.zip(tries).exists(x => x._1 > x._2)) db.safeQuit(s"One of the $hits expected hits is greater than one of the $tries tries found (tries = number os instances in the test set * (Q - |Y|) * runs).")
      println(db + ": " + hits.sum / tries.sum)
    }
  }

  run()
}
