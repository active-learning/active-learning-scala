package novo

import al.strategies.TU
import clean.lib.{CM, Ds, Global}
import ml.Pattern
import ml.classifiers.{Learner, Maj, RF}
import util.{Datasets, Stat}

import scala.util.Random

object DensityAttsExpAA extends Args with CM with DistT with AAInitializer {
  val context: String = "datt exp"
  lazy val exp = getClass.getSimpleName + "novo: " + args.filter(!_.startsWith("clear=")).sorted.mkString(" ")
  run()

  def processa(f: (String, Seq[Pattern], Seq[Pattern]) => (scala.Vector[Pattern], scala.Vector[Pattern]), parallel: Boolean)(dataset: String) = {
    def exe(patts: Vector[Pattern], preAdded: Boolean) = {
      val runs = (if (argb("parr")) (1 to argi("runs")).par else 1 to argi("runs")) map { run =>
        val shuffled = new Random(run).shuffle(patts)
        Datasets.kfoldCV(shuffled, argi("k"), parallel) { (pool, ts, fold, minSize) =>
          val step = run - 1 + "." + fold
          val seed = 1000 * run + fold
          val l = RF(seed, argi("trees"), argi("trees") / 2)
          if (pool.size<5) (0d -> 0d, 0d -> 0d)
          else {
            if (preAdded) (alc(l, pool, ts) -> -1d, -1d -> -1d)
            else {
              val ((newpool, newpoolOnlyDens), (newTs, newTsOnlyDens)) = f(dataset + step + "tr", pool, pool) -> f(dataset + step + "ts", ts, pool)
              //normal junto sozinho zeroR
              (alc(l, pool, ts) -> alc(l, newpool, newTs), alc(l, newpoolOnlyDens, newTsOnlyDens) -> alc(Maj(), pool, ts))
            }
          }
        }
      }
      val (la, lb) = runs.flatten.unzip
      val (nor, jun) = la.unzip
      val (soz, zer) = lb.unzip
      Seq(nor.toVector, jun.toVector, soz.toVector, zer.toVector) map Stat.media_desvioPadrao
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
          val nojusoze = exe(patts, preAdded = false)
          val juPreAdded = exe(newPatts, preAdded = true).filter(_._1 > -1d)
          val soPreAdded = exe(newPattsOnlyDens, preAdded = true).filter(_._1 > -1d)
          val res = (nojusoze ++ juPreAdded ++ soPreAdded) map (x => (1000 * x._1).round / 1000d + "/" + (1000 * x._2).round / 1000d + " ")
          println(res.mkString)
          alive.putResults(res.mkString)
          alive.stop()
        } else println("busy")
    }
  }

  def run() = try {
    Global.debug = argi("log")
    println(exp)
    val f = if (argb("1d")) addAtt1d _ else addAtt _
    argl.getOrElse("file", argl("datasets")) foreach processa(f, argb("parf"))
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      Thread.sleep(100)
      println(e.getClass.getName + " " + e.getMessage)
  }

  def alc(l: Learner, pool: Vector[Pattern], ts: Vector[Pattern]) = if (pool.size<10) 0d else {
    val s = TU(pool, l, pool)
    val labeled = initialSet(pool)
    val unlabeled = pool.diff(labeled)
    val queries = s.queries_noLabels(unlabeled, labeled).take(argi("q") - labeled.size).toVector
    val vs = 1 to argi("q") map { i =>
      val m = l.build(queries.take(i))
      accBal(m.confusion(ts))
    }
    vs.sum / vs.size
  }

}
