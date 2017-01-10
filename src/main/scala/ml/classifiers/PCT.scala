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

import java.io.{FileWriter, File}
import java.util.UUID

import clean.tex.MetaTrait
import clus.Clus
import ml.Pattern
import ml.models.{FakeModelRank, Model}
import util.Datasets

case class PCT(ntrees: Int = 1000, seed: Int = 42, trts: Vector[Pattern] = Vector()) extends Learner with MetaTrait {
  override val toString = "PCT"
  val id = 91919292
  val abr = toString

  def EMC(model: Model)(patterns: Seq[Pattern]) = ???

  def update(model: Model, fast_mutable: Boolean, semcrescer: Boolean = false)(pattern: Pattern) = ???

  def expected_change(model: Model)(pattern: Pattern) = ???

  def build(patterns0: Seq[Pattern]) = {
    val tr = patterns0.toVector
    val trSemParecidos = tr
    //clus; seed tb serve pra situar run e fold durante paralelização
    val id = UUID.randomUUID() + patterns0.map(_.id).mkString.hashCode.toString + (System.currentTimeMillis() % 1000000).toString
    val arqtr = s"/run/shm/cla-tr$seed$id"
    val arqtrts = s"/run/shm/cla-trts$seed$id"
    patts2file(trSemParecidos, arqtr)
    patts2file(trts, arqtrts)
    //acurácia reportada pelo arquivo.out vai ser superotimista, pois é sobre tr+ts!
    val f = new FileWriter(s"/run/shm/cla-clus$seed$id.s")
    f.write(clusSettings(ntrees, tr.head.nattributes, 1, seed, arqtr, arqtrts, 1))
    f.close()

    System.setOut(dummyStream)
    Clus.main(Array("-forest", "-silent", s"/run/shm/cla-clus$seed$id"))
    System.setOut(originalStream)

    //coleta predições de tr e ts num só arquivo
    val clusTSPredictionsARFF = Datasets.arff(s"/run/shm/cla-clus$seed$id.test.pred.arff", dedup = false, rmuseless = false) match {
      case Right(x) => x
      case Left(m) => error(s"${m} <- m \n " + s"/run/shm/cla-clus$seed$id.test.pred.arff")
    }

    new File(arqtr + ".arff").delete
    new File(arqtrts + ".arff").delete
    new File(s"/run/shm/cla-clus$seed$id.s").delete()
    new File(s"/run/shm/cla-clus$seed$id.train.1.pred.arff").delete
    new File(s"/run/shm/cla-clus$seed$id.test.pred.arff").delete
    new File(s"/run/shm/cla-clus$seed$id.out").delete
    new File(s"/run/shm/cla-clus$seed$id.model").delete

    lazy val clusTRTSRanks = clusTSPredictionsARFF.zip(trts).map { case (pa, pats) =>
      pats.id -> pa.array.drop(2).take(pats.nclasses)
      //      pats.id -> pa.array.zipWithIndex.flatMap { case (v, i) => if (pa.attribute(i).name.startsWith("Original-p")) Some(v) else None }
    }

    FakeModelRank(clusTRTSRanks.toMap)
  }

  val boundaryType = "flexível"
  val attPref = "ambos"
  lazy val context: String = "PCT"
}
