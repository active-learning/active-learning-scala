///*
//
//active-learning-scala: Active Learning library for Scala
//Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
//*/
//
//package clean.run.uti
//
//import al.strategies.{RandomSampling, Strategy}
//import clean.{Ds, Exp}
//import ml.Pattern
//import weka.filters.Filter
//
//import scala.collection.mutable
//
//object grpByQ extends Exp {
//  val arguments = superArguments
//  val context = "grpByQ"
//  val m = mutable.Map[String, (Int, Int)]()
//  val ignoreNotDone = false
//  run()
//
//  def strats(pool: Seq[Pattern], seed: Int) = List(RandomSampling(pool))
//
//  def isAlreadyDone(ds: Ds) = false
//
//  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
//  }
//
//  def datasetFinished(ds: Ds) {
//    if (ds.isQCalculated) {
//      m += ds.dataset ->(ds.Q, ds.n)
//      println(s"${ds.Q} ${ds.n} $ds")
//    }
//  }
//
//  def end(res: Map[String, Boolean]): Unit = {
//    m.toList.sortBy(_._2) foreach println
//    println(s"")
//    m.toList.sortBy(_._2).map(_._1) foreach println
//  }
//}
