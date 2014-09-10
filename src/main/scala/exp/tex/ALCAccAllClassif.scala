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

import app.ArgParser
import app.db.entities.Dataset
import exp.result.Res
import ml.classifiers.{VFDT, KNNBatch, NB}
import util.{StatTests, Stat, FriedmanNemenyiTest}

import scala.collection.mutable

object ALCAccAllClassif extends ALCAccTrait {
  val desc = s"Version ${ArgParser.version} \nPega ALCs da tabela 'res' e imprime tabela latex. Learner will be ignored!"
  val learners = Seq(NB(), VFDT(), NB(), VFDT(), KNNBatch(5, "eucl", Seq(), weighted = true))
  //  val learners = Seq(NB(), VFDT(), KNNBatch(5, "eucl", Seq(), "", weighted = true))
  //  val learners = Seq(NB(), KNNBatch(5, "eucl", Seq(), "", weighted = true))

  run()
}