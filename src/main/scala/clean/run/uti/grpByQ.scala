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

package clean.run.uti

import java.io.FileWriter

import al.strategies.RandomSampling
import clean.{Lock, Ds, Exp}
import ml.Pattern
import weka.filters.Filter

import scala.collection.mutable

object grpByQ extends Exp with Lock {
  val arguments = superArguments
  val context = "grpByQ"
  val m = mutable.Map[String, (Int, Int)]()
  val ignoreNotDone = false
  run()

  def strats(pool: Seq[Pattern], seed: Int) = List(RandomSampling(pool))

  def isAlreadyDone(ds: Ds) = false

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
  }

  def datasetFinished(ds: Ds) {
    if (ds.isQCalculated) {
      acquire()
      m += ds.dataset ->(ds.Q, (ds.n * 4) / 5)
      release()
    }
  }

  def end(res: Map[String, Boolean]): Unit = {
    println(s"")
    println(m.toList.sortBy(_._2).map(_._1).mkString(","))
    println(s"")
    val bla = m.toList.sortBy(_._2).map(_._1).zipWithIndex.groupBy { case (d, i) => i % 5} map (_._2)
    bla.zipWithIndex foreach { case (g, i) =>
      val fw = new FileWriter(s"s$i")
      g.foreach(x => fw.write(x._1 + "\n"))
      fw.close()
    }
  }
}
