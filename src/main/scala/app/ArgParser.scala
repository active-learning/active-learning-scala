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

import java.io.{FileWriter, File}

import ml.classifiers.{Learner, NB}
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
      lazy val lazyInsts = Datasets.arff(bina = true, debug = false)(path + name + ".arff") match {
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
      else println(className + " base-dir dataset1,dataset2,...,datasetn parallel(datasets,runs,folds):drf")
      sys.exit(0)
    }
    (args(0) + "/", args(1).split(",").toSeq)
  }

  def testArgsWithLearner(className: String, args: Array[String], text: String): (String, Seq[String], (Int, Int) => Learner) = {
    if (args.length != 4) {
      println("____________\n" + text + "\n------------\nUsage:")
      println(className + " base-dir dataset1,dataset2,...,datasetn parallel(datasets,runs,folds):drf learner")
      sys.exit(0)
    }
    def learner(Lmax: Int, seed: Int) = args(3) match {
      case "NB" => NB()
    }
    (args(0) + "/", args(1).split(",").toSeq, learner)
  }

  def testArgsWithText(className: String, args: Array[String], text: String) = {
    if (args.length < 4) {
      println("____________\n" + text + "\n------------\nUsage:")
      println(className + " base-dir dataset1,dataset2,...,datasetn parallel(datasets,runs,folds):drf text")
      sys.exit(0)
    }
    (args(0) + "/", args(1).split(",").toSeq, args.drop(3).mkString(" "))
  }
}
