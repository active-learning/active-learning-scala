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
package al.strategies

import java.io.FileWriter
import java.util.UUID

import ml.Pattern
import ml.classifiers.{NB, NoLearner}
import ml.clusterers.HC.HClusterer
import util.Datasets

import scala.util.Random

/**
 * Queries according the hierarchical clustering.
 * It assumes that the pool is already randomized.
 * It always queries all the pool, since the slow part is the clustering not the HS.
 * @param pool
 */
case class ClusterBased(pool: Seq[Pattern], debug: Boolean = false)
  extends StrategyAgnostic {
  override val toString = "Hierarchical Sampling"
  println("The executable file used for Cluster-based strategy is part of HS. Hierarchical Sampling (HS) version 1.0 see LICENSE GPL file.")
  //  println(
  //    """
  //      |The executable file used for Cluster-based strategy is part of HS.
  //      |
  //      |Hierarchical Sampling (HS) version 1.0
  //      |Daniel Hsu <djhsu@cs.ucsd.edu>
  //      |(C) Copyright 2008, Daniel Hsu
  //      |
  //      |HS is free software: you can redistribute it and/or modify it under the terms
  //      |of the GNU General Public License as published by the Free Software Foundation,
  //      |either version 3 of the License, or (at your option) any later version.
  //      |
  //      |HS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  //      |without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  //      |PARTICULAR PURPOSE.  See the GNU General Public License for more details.
  //      |
  //      |You should have received a copy of the GNU General Public License with this
  //      |program.  If not, see <http://www.gnu.org/licenses/>.
  //      |
  //      |
  //      |Based on work described in
  //      |
  //      |  Sanjoy Dasgupta and Daniel Hsu, Hierarchical sampling for active learning.
  //      |  Twenty-Fifth International Conference on Machine Learning, 2008.
  //      |
  //      |
  //    """.stripMargin)
  val learner = NoLearner()
  lazy val size = rest.length
  lazy val clusters = {
    println("Calling Weka WARD clusterer...")
    val r = HClusterer(rest)
    println(" Weka WARD clusterer called.")
    r
  }
  lazy val uuid = UUID.randomUUID() + "_" + pool.hashCode + hashCode() + "_" + System.currentTimeMillis + "_" + pool.head.hashCode()
  lazy val tree_file = "/tmp/ClusterBased" + uuid + ".tree"
  lazy val labels_file = "/tmp/ClusterBased" + uuid + ".labels"
  lazy val results = {
    val fw = new FileWriter(tree_file)
    fw.write(clusters.parent_vector.mkString("\n"))
    fw.close()
    val fw2 = new FileWriter(labels_file)
    fw2.write(rest.map(_.label).mkString("\n"))
    fw2.close()
    println("Calling external program...")
    import scala.sys.process._
    val s = Seq("/home/davi/wcs/als/external-software/hierarchical-al/sample", nclasses.toString, tree_file, labels_file, "foo").lines.map(_.toInt).toArray
    println(" external program called...")
    s
  }
  var unlabeledSize = if (pool.length > 0) rest.length else -1 //Strategy with empty pool exists only to provide its name.

  protected def next(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val selected = rest(results(size - unlabeledSize))
    unlabeledSize -= 1
    selected
  }

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    plot.zera()
    for (p <- distinct_pool) plot.bola(p.x, p.y, p.label.toInt, 9)
    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 9)
    if (selected != null) plot.bola(selected.x, selected.y, -1, 15)
    plot.mostra()
    Thread.sleep((delay * 10).round.toInt)
  }
}

object CTest extends App {
  def learner = NB()

  val patts = new Random(0).shuffle(Datasets.arff(true)("/home/davi/unversioned/experimentos/fourclusters.arff").right.get)
  //  val patts = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("abalone-11class").right.get).take(2000)
  val n = (patts.length * 0.5).toInt
  val s = ClusterBased(patts.take(2000))

  //  val m = learner.build(patts.take(n))
  //  println(m.accuracy(patts.drop(n)))

  val l = s.queries.toList
  var m = learner.build(l.take(patts.head.nclasses))
  val b = l.drop(patts.head.nclasses) foreach {
    q => m = learner.update(m)(q)
      println(m.accuracy(patts.drop(n)))
  }
}