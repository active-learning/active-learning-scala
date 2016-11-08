package novo

import al.strategies.{Rnd, TU}
import clean.lib.{CM, Ds, Global}
import ml.Pattern
import ml.classifiers.{Learner, RF}
import util.Datasets

import scala.util.Random

object ALCinversionStopCritExp extends Args with CM with DistT with AAInitializer {
  val context: String = "ALC inversion stopping criterion exp"
  lazy val exp = getClass.getSimpleName + ": " + args.filter(x => !x.startsWith("clear=") && !x.startsWith("log")).sorted.mkString(" ")
  run()

  def processa(parallel: Boolean)(dataset: String) = {
    def exe(patts: Vector[Pattern]) = {
      val runs = (if (argb("parr")) (1 to argi("runs")).par else 1 to argi("runs")) map { run =>
        val shuffled = new Random(run).shuffle(patts)
        Datasets.kfoldCV(shuffled, argi("k"), parallel) { (pool, ts, fold, minSize) =>
          val step = run - 1 + "." + fold
          val seed = 1000 * run + fold
          val l = RF(seed, argi("trees"), argi("trees") / 2)

          //          println(step)
          lc(l, pool, ts) //map (x => x._1 - x._2)

        }
      }
      runs.flatten.toList
    }
    print(dataset + " ")
    val alive = ALive(dataset, exp, disabled = false)
    if (argb("clear")) alive.clear()
    alive.getResults match {
      case Some(str) => println(str)
      case None =>
        if (alive.isFree) {
          alive.start()
          val ds = Ds(dataset, readOnly = true)
          ds.open()
          println(s"$dataset ------------------------")
          val ret = exe(ds.patterns)
          val ret2 = ret.reduce { (lista, listb) =>
            lista.zip(listb) map { case ((a1, a2), (b1, b2)) => a1 + b1 -> (a2 + b2) }
          }
          println(s"$dataset ==========================")
          val res = ret2 map { case (a, b) =>
            s"${(a / ret.size * 100).round / 100d} ${(b / ret.size * 100).round / 100d}\n"
          }
          ds.close()

          println(res.mkString)
          alive.putResults(res.mkString)

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

  def lc(l: Learner, pool: Vector[Pattern], ts: Vector[Pattern]) = if (pool.size < 10) sys.error("pool.size < 10")
  else {
    val s = argt("strat") match {
      case "tu" => TU(pool, l, pool)
      case "rnd" => Rnd(pool)
      case x => sys.error(s"Strategy $x not reconized.")
    }

    val labeled = initialSet(pool)
    val unlabeled = pool.diff(labeled)
    val queries = s.queries_noLabels(unlabeled, labeled).take(argi("q") - labeled.size).toVector
    val vs = 1 to argi("q") map { fim =>
      val queriesrev = queries.take(fim).reverse
      val accs = 1 to fim map (i => accBal(l.build(queries.take(i)).confusion(ts)))
      val accsrev = 1 to fim map (i => accBal(l.build(queriesrev.take(i)).confusion(ts)))
      accs.sum / accs.size -> accsrev.sum / accsrev.size
    }
    vs
  }

}
