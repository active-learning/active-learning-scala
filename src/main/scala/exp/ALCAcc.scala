///*
// active-learning-scala: Active Learning library for Scala
// Copyright (c) 2014 Davi Pereira dos Santos
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
// */
//
//package exp
//
//import app.ArgParser
//import app.db.ClassName
//import app.db.entities.Dataset
//import al.strategies._
//import ml.Pattern
//
//object ALCAcc extends App with ClassName {
//  val desc = s"Version ${ArgParser.version} \nCalcula ALCs e coloca na tabela 'res'."
//  val (path, datasetNames0, learner) = ArgParser.testArgsWithLearner(className, args, desc)
//  val parallel = args(2) == "y"
//  val readOnly = true
//  val runs = 5
//  val folds = 5
//  val dest = Dataset(path, createOnAbsence = false, readOnly) _
//
//  def strats(run: Int, pool: Seq[Pattern]) = List(
//    Uncertainty(learner(run, pool), pool),
//    Entropy(learner(run, pool), pool),
//    Margin(learner(run, pool), pool),
//    new SGmulti(learner(run, pool), pool, "consensus"),
//    new SGmulti(learner(run, pool), pool, "majority"),
//    new SGmultiJS(learner(run, pool), pool),
//    DensityWeighted(learner(run, pool), pool, 1, "eucl"),
//    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "cheb"),
//    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "eucl"),
//    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "maha"),
//    DensityWeightedTrainingUtility(learner(run, pool), pool, 1, 1, "manh"),
//    MahalaWeighted(learner(run, pool), pool, 1),
//    MahalaWeightedTrainingUtility(learner(run, pool), pool, 1, 1)
//  )
//
//  val qname = (if (parallel) datasetNames.par else datasetNames).toList map { datasetName =>
//    val db = dest(datasetName)
//    if (db.dbOriginal.exists()) {
//      db.open()
//      val Q = db.Q
//      val timeSteps = (Q - db.nclasses) * db.nclasses * db.nclasses
//      val hitsAcumulados = "select sum(value) from hit where strategyid=1 and learnerid=3 and pred=expe and position<4 group by run,fold"
//      val sizesAcumulados = "select sum(value) from hit where strategyid=1 and learnerid=3 and position<4 group by run,fold"
//      ???
//      println(s"$Q $datasetName ${db.n}")
//      db.close()
//      (Q, datasetName)
//    } else (-1, datasetName)
//  }
//  qname.sortBy(_._1) foreach println
//  println("")
//  println(qname.sortBy(_._1).map(_._2).mkString(","))
//  println("")
//}
