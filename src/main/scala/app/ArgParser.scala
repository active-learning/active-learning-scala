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

package app

import java.io.File

import ml.Pattern
import ml.classifiers._
import util.Datasets
import weka.core.Instances

import scala.util.{Failure, Success, Try}

object ArgParser {
  //  lazy val majorVersion = scala.io.Source.fromFile("majV").getLines().toList.head
  //  lazy val minorVersion = scala.io.Source.fromFile("minV").getLines().toList.head
  lazy val version = scala.io.Source.fromFile("VERSION").getLines().toList.head
  val appPath = new File(".").getCanonicalPath + "/"

  def applyArgs(args: Array[String])(f: (String, => Option[Instances]) => Unit) {
    val path = args(0) + "/"
    val names = args(1).split(",").toList
    val par = args.length > 2 && args(2) == "y"
    (if (par) names.par else names).foreach { name =>
      lazy val lazyInsts = Datasets.arff(bina = true, debug = false)(path + name + ".arff", zscored = false) match {
        case Left(str) => println(str + "\nSkipping dataset '" + name + "'."); None
        case Right(data) =>
          println("Processing '" + name + "':")
          val insts = new Instances(data.head.dataset(), data.length)
          data foreach insts.add
          Some(insts)
      }
      Try(f(name, lazyInsts)) match {
        case Success(_) => println("'" + name + "' ready!")
        case Failure(ex) => println(ex + "\nSkipping dataset '" + name + "'.")
      }
    }
  }

  def testArgs(className: String, args: Array[String], numArgs: Int, text: String) = {
    if (args.length != numArgs) {
      println("____________\n" + text + "\n------------\nUsage:")
      if (numArgs == 2) println(className + " base-dir dataset1,dataset2,...,datasetn")
      else println(className + " base-dir dataset1,dataset2,...,datasetn parallel(datasets,runs,folds):drf\n" +
        "Parallel means 'to parallelize datasets, but serialize runs and folds.")
      sys.exit(1)
    }
    (args(0) + "/", args(1).split(",").toSeq)
  }

  def testArgsWithLearner(className: String, args: Array[String], text: String): (String, Seq[String], (Int, Int, Seq[Pattern]) => Learner) = {
    if (args.length != 4) {
      println("____________\n" + text + "\n------------\nUsage:")
      println(className + " base-dir dataset1,dataset2,...,datasetn parallel(datasets,runs,folds):drf learner")
      sys.exit(1)
    }
    def learner(Lmax: Int, seed: Int, pool: Seq[Pattern]) = args(3) match {
      case "NB" => NB()

      case "CI" => CIELM(seed)
      case "ECI" => ECIELM(seed)
      case "I" => IELM(seed)
      case "EI" => EIELM(seed)

      case "intera" => interaELM(Lmax / 3, seed)
      //      case "interaw" => interawELM(Lmax / 3, seed)

      case "C45" => C45()
      case "VFDT" => VFDT()
      case "1NNc" => KNN(1, "cheb", pool)
      case "1NNe" => KNN(1, "eucl", pool)
      case "1NNm" => KNN(1, "manh", pool)
      case "3NNc" => KNN(3, "cheb", pool)
      case "3NNe" => KNN(3, "eucl", pool)
      case "3NNm" => KNN(3, "manh", pool)
      case "5NNc" => KNN(5, "cheb", pool)
      case "5NNe" => KNN(5, "eucl", pool)
      case "5NNm" => KNN(5, "manh", pool)
    }
    (args(0) + "/", args(1).split(",").toSeq, learner)
  }

  def testArgsWithText(className: String, args: Array[String], text: String) = {
    if (args.length < 4) {
      println("____________\n" + text + "\n------------\nUsage:")
      println(className + " base-dir dataset1,dataset2,...,datasetn parallel(datasets,runs,folds):drf text")
      sys.exit(1)
    }
    (args(0) + "/", args(1).split(",").toSeq, args.drop(3).mkString(" "))
  }

  def testArgsWithTextNoPar(className: String, args: Array[String], text: String) = {
    if (args.length != 3) {
      println("____________\n" + text + "\n------------\nUsage:")
      println(className + " base-dir dataset1,dataset2,...,datasetn collection-name(uci, ...)")
      sys.exit(1)
    }
    (args(0) + "/", args(1).split(",").toSeq, args(2))
  }
}
