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

import al.strategies.Strategy
import clean.lib.Ds
import ml.Pattern
import ml.models.Model

case class BestClassifCV100ReadOnly(ds: Ds, r: Int, f: Int, s: Strategy) extends Learner {
   override lazy val toString = s"Bcv100($ds,${s.limpa},$r,$f): $classif"
   lazy val abr = classif.abr
   lazy val attPref = classif.attPref
   lazy val boundaryType = classif.boundaryType
   lazy val id = ds.read(s"select c from classif100 where s=${s.id} and l=${s.learner.id} and r=$r and f=$f") match {
      case List(Vector(x)) => x.toInt
      case x => throw new Exception(s"classif ${s.learner} não encontrado, retornou: $x")
   }
   lazy val classif = learners.find(_.id == id).get
   lazy val learners = Seq(
      KNNBatcha(5, "eucl", Seq(), weighted = true)
      , C45()
      , RF(42)
      , NBBatch()
      , CIELMBatch(42)
      , SVMLibRBF(42)
   )

   def update(model: Model, fast_mutable: Boolean, semcrescer: Boolean)(pattern: Pattern) = ???

   def expected_change(model: Model)(pattern: Pattern) = ???

   def build(pool: Seq[Pattern]) = ???
}
