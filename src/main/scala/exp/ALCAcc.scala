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
import app.db.entities.Dataset

object ALCAcc extends Res {
  val desc = s"Version ${ArgParser.version} \nCalcula ALCs e coloca na tabela 'res'."

  def core(db: Dataset) {
    val Q = db.Q
    val timeSteps = (Q - db.nclasses) * db.nclasses * db.nclasses
    val hitsAcumulados = "select sum(value) from hit where strategyid=1 and learnerid=3 and pred=expe and position<4 group by run,fold"
    val sizesAcumulados = "select sum(value) from hit where strategyid=1 and learnerid=3 and position<4 group by run,fold"
  }

  run()
}
