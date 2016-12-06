package novo

import clean.lib.{CM, Ds, Global}
import ml.Pattern
import ml.classifiers.{Learner, Maj, RF}
import util.{Datasets, Stat}

import scala.util.Random

object DensityAttsExpAAComparaAcertos extends Args with CM with DistT with AAInitializer {
  lazy val neigs = argl("neigs").map(_.toInt)
  val context: String = "datt exp"
  lazy val exp = getClass.getSimpleName + "novo: " + args.filter(!_.startsWith("clear=")).filter(!_.startsWith("par")).filter(!_.startsWith("dry=" +
    "")).filter(!_.startsWith("log")).filter(!_.startsWith("datasets")).sorted.mkString(" ")
  run()

  def processa(f: (String, Seq[Pattern], Seq[Pattern]) => (scala.Vector[Pattern], scala.Vector[Pattern]), parallel: Boolean)(dataset: String) = {
    def exe(patts: Vector[Pattern]) = {
      val runs = (if (argb("parr")) (1 to argi("runs")).par else 1 to argi("runs")) map { run =>
        val shuffled = new Random(run).shuffle(patts)
        Datasets.kfoldCV(shuffled, argi("k"), parallel) { (pool, ts, fold, minSize) =>
          val step = run - 1 + "." + fold
          val seed = 1000 * run + fold
          val l = RF(seed, argi("trees"), argi("trees") / 2)
          if (pool.size < 5) 0d
          else {
            val ((_, densPool), (_, densTs)) = f(dataset + step + "tr", pool, pool) -> f(dataset + step + "ts", ts, pool)
            val s = strat(pool, l)
            val labeled = initialSet(pool)
            val unlabeled = pool.diff(labeled)
            val queries = s.queries_noLabels(unlabeled, labeled).take(argi("q") - labeled.size).toVector
            val m = l.build(queries.take(argi("q")))
            val origHits = ts flatMap (p => if (m.hit(p)) Some(p.id) else None)

            val densQueries = pool filter queries.contains
            val densM = l.build(densQueries)
            val densHits = densTs flatMap (p => if (densM.hit(p)) Some(p.id) else None)

            densHits.diff(origHits).size / ts.size.toDouble
          }
        }
      }
      Stat.media_desvioPadrao(runs.flatten.toVector)
    }

    print(dataset + " ")
    val alive = ALive(dataset, exp)
    if (argb("clear")) {
      println("Apagando...")
      alive.clear()
    }
    alive.getResults match {
      case Some(str) => println(str)
      case None if argb("dry") => println("...")
      case None if alive.isFree =>
        alive.start()
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        val patts = ds.patterns
        ds.close()
        val res = exe(patts)
        println(res)
        alive.putResults(res._1 + " " + res._2)
        alive.stop()
      case _ => println("busy")
    }
  }

  def run() = try {
    Global.debug = argi("log")
    println(exp)
    println("normal junto sozinho zeroR    junto-preadd sozinho-preadd")
    val f = if (argb("1d")) addAtt1d _ else addAtt _
    argl.getOrElse("file", argl("datasets")) foreach processa(f, argb("parf"))
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      Thread.sleep(100)
      println(e.getClass.getName + " " + e.getMessage)
  }
}
