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

package ml.classifiers

import java.io.{File, FileWriter}
import java.util.UUID

import clean.tex.MetaTrait
import clus.Clus
import ml.Pattern
import ml.models.{EnsembleModel, ELMModel, FakeModelRank, Model}
import util.Datasets

case class PCTELM(ntrees: Int = 1000, seed: Int = 42, trts: Vector[Pattern] = Vector()) extends Learner with MetaTrait {
  override val toString = "PE"
  val id = 71939292
  val abr = toString
  val pct = PCT(ntrees, seed, trts)

  def EMC(model: Model)(patterns: Seq[Pattern]) = ???

  def update(model: Model, fast_mutable: Boolean, semcrescer: Boolean = false)(pattern: Pattern) = ???

  def expected_change(model: Model)(pattern: Pattern) = ???

  def build(patterns0: Seq[Pattern]) = {
    val me = (1 to ntrees).foldLeft(FakeModelRank(Map())) { (fm, seedinc) =>
      val elm = NinteraELM(seed + seedinc * 10000)
      var m0 = elm.batchBuild(patterns0).asInstanceOf[ELMModel]
      val L = elm.LForMeta(m0, LOO = false)
      m0 = elm.batchBuild(patterns0).asInstanceOf[ELMModel]
      elm.fullBuildForMeta(L, m0)
      fm + FakeModelRank((trts map (x => x.id -> m0.output(x).clone())).toList.toMap)
    }

    val mp = pct.build(patterns0)

    EnsembleModel(Seq(me, mp))
  }

  val boundaryType = "flexível"
  val attPref = "ambos"
  lazy val context: String = "PCT"
}
