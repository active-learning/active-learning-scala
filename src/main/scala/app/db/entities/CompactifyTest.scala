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

package app.db.entities

import java.util.Calendar

import al.strategies.{RandomSampling, Strategy}
import ml.Pattern
import ml.classifiers._
import util.{Lazy, ALDatasets, Datasets, Tempo}
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable
import scala.util.Random


/**
 * Verifica se compactação destroi sequencia de ids ou sequencia de queries (tem 'position' então é dificil).
 */
object CompactifyTest extends App {
  //load patterns
  //  val patts = Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("iris").right.get

  //reorder patterns as queries
  //  val shuffled = patts.drop(5) ++ patts.take(4)

  //  val d = Dataset("/home/davi/wcs/ucipp/uci/")("arcene")
  val d = Dataset("/home/davi/wcs/ucipp/uci/")("abalone-3class")
  d.open()
  //  d.saveQueries(RandomSampling(patts), 64, 17, 0.2)

  //load queries as patterns
  val qpattss = ALDatasets.queriesFromSQLite(Dataset("/home/davi/wcs/ucipp/uci/")("abalone-3class"))(RandomSampling(Seq()), 0, 0) match {
    case Right(x) => x
    case Left(str) => println(s"Problema: $str"); ???
  }
  qpattss take 10 foreach println

  //  val qpattsa = ALDatasets.queriesFromARFF("/home/davi/wcs/ucipp/uci/iris.arff")(d)(RandomSampling(Seq()), 0, 0,) match {
  //    case Right(x) => x
  //    case Left(str) => println(s"Problema: $str"); ???
  //  }


  //  d.exec("select  rowid,* from inst").get foreach println
  println(d.compactify())
  d.close()

}