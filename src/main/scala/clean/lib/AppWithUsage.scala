package clean.lib

import java.io.FileWriter
import java.security.SecureRandom
import java.util.UUID

import al.strategies.Strategy
import ml.Pattern
import ml.classifiers.Learner
import util.{Datasets, XSRandom}
import weka.attributeSelection.{WrapperSubsetEval, CfsSubsetEval, GreedyStepwise}
import weka.classifiers.trees.J48
import weka.core.Instances
import weka.filters.Filter
import weka.filters.supervised.attribute.AttributeSelection

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

trait AppWithUsage extends App with Log with ArgParser {
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
  lazy val passivas = args(4).contains("p")
  lazy val porRank = args(5).contains("r")
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

  def ff(precision: Double)(x: Double) = (x * precision).round / precision

  def maxQueries(ds: Ds) = math.max(ds.nclasses, math.min(ds.expectedPoolSizes(folds).min, maxQueries0))

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

  def renomeia(ds: Ds) = {
    val reticencias = if (ds.dataset.size > 18) "..." else ""
    val name = ds.dataset.take(5) match {
      case "heart" => ds.dataset.replace("processed-", "")
      case "conne" => ds.dataset.replace("connectionist", "connect.")
      case _ => ds.dataset
    }
    name.take(18).split("-").mkString(" ") + reticencias
  }

  /**
   * pega n melhores e todas as que empatarem com a n melhor
   */
  def pegaMelhores[T](s: Seq[T], n: Int)(f: T => Double) = {
    var t = 0
    val (bef, aft) = s.groupBy(f).toList.sortBy(_._1).reverse.span { case (k, vs) =>
      t += vs.size
      t < n
    }
    (bef :+ aft.head).map(_._2).flatten
  }

  /**
   * dispensa n melhores e todas as que empatarem com a n melhor
   */
  def dispensaMelhores[T](s: Seq[T], n: Int)(f: T => Double) = {
    var t = 0
    s.groupBy(f).toList.sortBy(_._1).reverse.dropWhile { case (k, vs) =>
      t += vs.size
      t < n
    }.tail.map(_._2).flatten
  }


  def accsPerPool(ds: Ds, s0: (Learner) => Strategy, learners: Seq[Learner], measure: (Strategy, Learner, Int, Int) => Measure) = for {
    r <- 0 until runs
    f <- 0 until folds
  } yield for (le <- learners) yield {
      val s = s0(le)
      (le, measure(s, le, r, f).read(ds).getOrElse(error("NA:" +(ds, s.abr, le, r, f) + "NA:" +(ds, s.id, le.id, r, f))), r, f)
    }


  //   def winnersPerPool(ds: Ds, s0: (Learner) => Strategy, learners: Seq[Learner], measure: (Strategy, Learner, Int, Int) => Measure) = {
  //      val r = for (le <- learners) yield {
  //         val s = s0(le)
  //         val accs = for {
  //            r <- 0 until runs
  //            f <- 0 until folds
  //         } yield measure(s, le, r, f).read(ds).getOrElse(error("NA:" +(ds, s.abr, le, r, f) + "NA:" +(ds, s.id, le.id, r, f)))
  //         le -> accs.sum
  //      }
  //      r.sortBy(_._2)
  //   }

  def DsByMinSize(lst: List[String], size: Int) = lst.toList.filter { dataset =>
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val r = ds.poolSize >= size
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

  def arff(exemplos: Seq[(Seq[(String, Double, String)], String)], print: Boolean = false) = {
    val labels = exemplos.map(_._2).distinct.sorted
    val classAtt = "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}"
    val attsNameType = exemplos.head._1 map { case (no, va, ty) => s"$no $ty" }
    val header = List(s"@relation autogeneratedby$context") ++ (attsNameType map (nt => s"@attribute $nt")) :+ classAtt
    val data = exemplos.map(x => x._1.map(_._2).mkString(",") + s",${x._2}")
    if (print) {
      println("labels: " + labels.mkString(" "))
      println(s"#classes: ${labels.size}")
      println(s"#atributos: ${exemplos.head._1.size}")
      println(s"#exemplos: ${exemplos.size}")
    }
    header ++ Seq("@data") ++ data
  }

  def grava(arq: String, linhas: Seq[String], print: Boolean = false): Unit = {
    val fw = new FileWriter(arq)
    linhas foreach (x => fw.write(s"$x\n"))
    fw.close()
    if (print) linhas foreach println
  }

  def cv10x10fold(bagsFromFile: Vector[Vector[Pattern]], leas: Vector[Learner]) = (1 to 10).par map { run =>
    val shuffledbagsFromFile = new Random(run).shuffle(bagsFromFile)
    Datasets.kfoldCV2(shuffledbagsFromFile, 10, parallel = true) { (trbags, tsbags, fold, minSize) =>
      val tr = trbags.flatten
      //refaz bags por duplicidade
      val bags = tsbags.flatten.groupBy(x => x.vector)
      val bagssize = bags.size.toDouble
      leas map { le =>
        val m = le.build(tr)
        (bags.map(_._2) map { tsbag =>
          tsbag.map(_.label).contains(m.predict(tsbag.head))
        }).count(_ == true) / bagssize
      }
    }
  }

  def cv10x10foldFS(bagsFromFile: Vector[Vector[Pattern]], leas: Vector[Learner]) = (1 to 1).par map { run =>
    val shuffledbagsFromFile = new Random(run).shuffle(bagsFromFile)
    Datasets.kfoldCV2(shuffledbagsFromFile, 8, parallel = true) { (trbags, tsbags, fold, minSize) =>
      val tr0 = trbags.flatten
      val ts0 = tsbags.flatten

      val filter = new AttributeSelection()
      //      val eval = new CfsSubsetEval()
      val eval = new WrapperSubsetEval()
      eval.setClassifier(new J48())
      val search = new GreedyStepwise()
      search.setSearchBackwards(true)
      filter.setEvaluator(eval)
      filter.setSearch(search)
      filter.setInputFormat(tr0.head.dataset())
      val tr = Datasets.instances2patterns(Filter.useFilter(Datasets.patterns2instances(tr0), filter))
      val ts = Datasets.instances2patterns(Filter.useFilter(Datasets.patterns2instances(ts0), filter))

      //refaz bags por duplicidade
      val bags = ts.groupBy(x => x.vector)
      val bagssize = bags.size.toDouble
      leas map { le =>
        val m = le.build(tr)
        (bags.map(_._2) map { tsbag =>
          tsbag.map(_.label).contains(m.predict(tsbag.head))
        }).count(_ == true) / bagssize
      }
    }
  }
}
