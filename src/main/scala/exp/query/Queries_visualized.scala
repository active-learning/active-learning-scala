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

package exp.query

import al.strategies._
import clean.lib.Ds
import ml.Pattern
import ml.classifiers._
import ml.models.Model
import util.{ALDatasets, Datasets, Tempo}

import scala.util.Random

object Queries_visualized extends App {
  //  val arff = "/home/davi/unversioned/experimentos/fourclusters.arff"
  val arff = "/home/davi/Dropbox/git/als/banana.arff"
  val data = new Random().shuffle(Datasets.arff(arff).right.get)
  //  val data= new Random().shuffle(Ds("banana", readOnly = true).patterns)
  val train = data.take(1000)
  val ts = data.drop(1000)
  var labeled = List[Pattern]()
  var model: Model = null

  val cc = RF(5,threads = 4)
  //KNNBatcha(75, "eucl", train, weighted = true)//SVMLibRBF() //RF(100)
//  val e = TU(train, cc, train, debug = true)
//  val e = EER(cc,train,"entropy")
    val e = IgnUncDen(cc, train, 80, train, debug = true)
  //  val e = DenAlternIgnUnc(cc, train, train.size/10, train, debug = true)


  Tempo.start
  for ((q, i) <- e.queries.zipWithIndex) {
    labeled = q :: labeled
    if (labeled.length <= labeled.head.nclasses) {
      if (labeled.length == labeled.head.nclasses) model = cc.build(labeled)
    } else {
      model = cc.update(model)(q)
      val acc = model.accuracy(ts)
      //         println(" " + acc)
      if (acc > 7) {
        //0.82
        Tempo.print_stop
        Thread.sleep(1000)
        sys.exit(1)
      }
      Thread.sleep(100)
    }
  }
}
