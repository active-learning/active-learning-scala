package clean.lib

import java.io.{File, FileWriter}
import java.security.SecureRandom
import java.util.UUID

import al.strategies.Strategy
import ml.Pattern
import ml.classifiers.{KNNBatcha, Learner}
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.{Stat, Datasets, XSRandom}
import weka.attributeSelection.{WrapperSubsetEval, CfsSubsetEval, GreedyStepwise}
import weka.classifiers.`lazy`.IBk
import weka.classifiers.trees.J48
import weka.core.Instances
import weka.core.converters.ArffSaver
import weka.filters.Filter
import weka.filters.supervised.attribute.AttributeSelection
import weka.filters.unsupervised.attribute.PrincipalComponents

import scala.util.Random

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

trait AppWithUsage extends App with Log with ArgParser with FilterTrait with Rank {
  val language = "pt"
  val NA = -9d
  val superArguments = List("debug-verbosity:-1,0,1,2,...,30", "files-with-dataset-names-or-dataset-names:file1,file2|#d1,d2,d3", "paralleliz(runs folds):r|f|rf|d", "maxtimesteps")
  val arguments: List[String]
  lazy val runs = Global.runs
  lazy val folds = Global.folds
  lazy val debugIntensity = if (args.isEmpty) 20 else args(0).toInt
  lazy val maxQueries0 = args(3).toInt
  lazy val sql = args(4)
  //   lazy val pesadas = args(4).contains("p")
  //   lazy val todas = args(4).contains("t")
  //  lazy val passivas = args(4).contains("p")
  lazy val porRank = args(4).contains("r")
  lazy val ntrees = args(5).toInt
  //  lazy val apenasUmPorBase = args(7).contains("um")
  lazy val criterio = args(6).toInt
  lazy val (rus, ks) = args(7).toInt -> args(8).toInt
  lazy val (ini, fim) = args(9) -> args(10)
  //  lazy val featureSel = args(13)
  //  lazy val smote = args(14) == "smote"
  //  lazy val smotePropor = args(15).toInt
  //  lazy val suav = args(16) == "su"
  //  lazy val semR = args(17)

  lazy val reduz = args(5).contains("r")
  lazy val comprimento = args(5)
  lazy val porRisco = args(6).contains("r")
  lazy val dist = args(7)
  lazy val normalizar = args(4).contains("y")
  lazy val path = args(4) + "/"
  lazy val trulyrnd = new SecureRandom()
  lazy val seed = trulyrnd.nextInt() + System.nanoTime() + System.currentTimeMillis() + UUID.randomUUID().toString.map(_.toByte).map(_.toInt).sum
  lazy val xsrnd = {
    val tmp = new XSRandom()
    tmp.setSeed(seed)
    tmp
  }
  lazy val rnd = new Random(xsrnd.nextInt())
  lazy val datasets = if (args(1).startsWith("@")) datasetsFromFiles(args(1).drop(1)).toList
  else rnd.shuffle(if (args(1).startsWith("#")) args(1).drop(1).split(',').toList else datasetsFromFiles(args(1)).toList)
  //  lazy val datasets = if (args(1).startsWith("#")) args(1).drop(1).split(',') else datasetsFromFiles(args(1))
  lazy val parallelRuns = args(2).contains("r")
  lazy val parallelFolds = args(2).contains("f")
  lazy val parallelDatasets = args(2).contains("d")
  lazy val learnerStr = if (args.size < 5) "learner-undefined" else args(4)
  lazy val learnersStr = if (args.size < 5) Array("learners-undefined") else args(4).split(",")
  lazy val memlimit = Global.memlimit


  def memoryMonitor() = {
    Global.running = true
    new Thread(new Runnable() {
      def run() {
        while (Global.running) {
          //60s
          1 to 300 takeWhile { _ =>
            Thread.sleep(200)
            Global.running
          }
          log(s"Memory usage: ${Runtime.getRuntime.totalMemory() / 1000000d}MB.", 30)
          //          if (Runtime.getRuntime.totalMemory() / 1000000d > memlimit) {
          //            Global.running = false
          //            error(s"Limite de $memlimit MB de memoria atingido.")
          //          }
        }
        log("Saiu do monitoramento de memória.", 30)
      }
    }).start()
  }

  def run() {
    try {
      Global.debug = debugIntensity
      println(args.mkString(" "))
      if (args.size != arguments.size) {
        println(s"Usage: java -cp your-path/als-version.jar ${this.getClass.getCanonicalName.dropRight(1)} ${arguments.mkString(" ")}")
        sys.exit(1)
      }
    } catch {
      case ex: Throwable => Global.running = false
        ex.printStackTrace()
        justQuit("Erro: " + ex.getMessage)
    }
  }


  def DsBy(lst: List[String], size: Int, onlyBinaryProblems: Boolean, notBinary: Boolean = false) = lst.toList.filter { dataset =>
    if (notBinary && onlyBinaryProblems) ???
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val r = ds.poolSize >= size && (notBinary && ds.nclasses != 2 || !notBinary && (!onlyBinaryProblems || onlyBinaryProblems && ds.nclasses == 2))
    ds.close()
    r
  }

  def binaryDs(lst: List[String]) = lst.toList.filter { dataset =>
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val r = ds.nclasses == 2
    ds.close()
    r
  }

  def renomeia(ds: Ds) = {
    val reticencias = if (ds.dataset.size > 18) "..." else ""
    val name = ds.dataset.take(5) match {
      case "heart" => ds.dataset.replace("processed-", "")
      case "conne" => ds.dataset.replace("connectionist", "connect.")
      case _ => ds.dataset
    }
    name.take(18).split("-").mkString(" ") + reticencias
  }
}
