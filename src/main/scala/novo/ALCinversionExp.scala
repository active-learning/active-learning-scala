package novo

import al.strategies.TU
import clean.lib.{CM, Ds, Global}
import ml.Pattern
import ml.classifiers.{Learner, Maj, RF}
import util.{Datasets, Stat}

import scala.util.Random

object ALCinversionExp extends Args with CM with DistT with AAInitializer {
  val context: String = "ALC inversion exp"
  lazy val exp = getClass.getSimpleName + ": " + args.filter(!_.startsWith("clear=")).sorted.mkString(" ")
  run()

  def processa(parallel: Boolean)(dataset: String) = {
    def exe(patts: Vector[Pattern], preAdded: Boolean) = {
      val runs = (if (argb("parr")) (1 to argi("runs")).par else 1 to argi("runs")) map { run =>
        val shuffled = new Random(run).shuffle(patts)
        Datasets.kfoldCV(shuffled, argi("k"), parallel) { (pool, ts, fold, minSize) =>
          val step = run - 1 + "." + fold
          val seed = 1000 * run + fold
          val l = RF(seed, argi("trees")) //, argi("trees") / 2)

          alc(l, pool, ts) foreach println

        }
      }
    }

    print(dataset + " ")
    val alive = ALive(dataset, exp)
    if (argb("clear")) alive.clear()
    alive.getResults match {
      case Some(str) => println(str)
      case None =>
        if (alive.isFree) {
          alive.start()
          val ds = Ds(dataset, readOnly = true)
          ds.open()
          val (patts, (newPatts, newPattsOnlyDens)) = ds.patterns -> f(dataset, ds.patterns, ds.patterns)
          ds.close()

//          println(res.mkString)
//          alive.putResults(res.mkString)

          alive.stop()
        } else println("busy")
    }
  }

  def run() = try {
    Global.debug = argi("log")
    println(exp)
    argl.getOrElse("file", argl("datasets")) foreach processa(argb("parf"))
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      Thread.sleep(100)
      println(e.getClass.getName + " " + e.getMessage)
  }

  def alc(l: Learner, pool: Vector[Pattern], ts: Vector[Pattern]) = if (pool.size < 10) List()
  else {
    val s = TU(pool, l, pool)
    val labeled = initialSet(pool)
    val unlabeled = pool.diff(labeled)
    val queries = s.queries_noLabels(unlabeled, labeled).take(argi("q") - labeled.size).toVector
    val vs = 1 to argi("q") map { i =>
      val m = l.build(queries.take(i))
      accBal(m.confusion(ts))
    }
    vs.toList
  }

}
