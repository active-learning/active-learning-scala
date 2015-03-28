/*
mls: basic machine learning algorithms for Scala
Copyright (C) 2014 Davi Pereira dos Santos

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
package ml.classifiers

import al.strategies.Passive
import clean.lib.{Global, Kappa, Ds}
import ml.Pattern
import ml.models.Model
import util.Stat

case class BestLearner(ds: Ds, seed: Int, pool: Seq[Pattern]) extends Learner {
   override lazy val toString = s"BestLearner: ${learner.limpa} para $ds"
   lazy val id = learner.id
   val abr: String = learner.abr
   val attPref: String = learner.attPref
   val boundaryType: String = learner.boundaryType
   lazy val learners = Seq(
      KNNBatcha(5, "eucl", pool, weighted = true)
      , C45()
      , RF(seed)
      , NBBatch()
      , CIELMBatch(seed)
      , SVMLibRBF(seed)
   )
   lazy val learner = learners.map { l =>
      val vs = for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield Kappa(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
      l -> Stat.media_desvioPadrao(vs.toVector)._1
   }.maxBy(_._2)._1
   lazy val querFiltro = learner.id match {
      case 2651110 => true //rbf
      case 8001 => true //ci
      case 773 => false //rf
      case 2 => false //knn
      case 12 => false //nb
      case 666003 => false //c45
   }


   def update(model: Model, fast_mutable: Boolean, semcrescer: Boolean)(pattern: Pattern) = learner.update(model, fast_mutable, semcrescer)(pattern)

   def expected_change(model: Model)(pattern: Pattern) = learner.expected_change(model)(pattern)

   def build(pool: Seq[Pattern]) = learner.build(pool)
}
