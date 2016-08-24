package novo

import clean.lib.{CM, Ds, Global}
import ml.Pattern
import ml.classifiers.{Maj, RF}
import util.{Datasets, Stat}

import scala.io.Source
import scala.util.Random

object DensityAttsExp extends Args with CM with DistT {
  val context: String = "datt exp"
  run()

  def processa(parallel: Boolean)(dataset: String) = {
    def exe(patts: Vector[Pattern], preAdded: Boolean) = {
      val runs = (1 to argi("runs")).par map { run =>
        val shuffled = new Random(run).shuffle(patts)
        Datasets.kfoldCV(shuffled, argi("k"), parallel) { (tr, ts, fold, minSize) =>
          val step = run - 1 + "." + fold
          val seed = 1000 * run + fold
          val l = RF(seed, argi("trees"), threads = 4)
          if (tr.isEmpty) (0d -> 0d, 0d -> 0d)
          else {
            if (preAdded) {
              val m = l.build(tr)
              (accBal(m.confusion(ts)) -> -1d, -1d -> -1d)
            } else {
              val ((newTr, newTrOnlyDens), (newTs, newTsOnlyDens)) = addAtt(dataset + step + "tr", tr, tr) -> addAtt(dataset + step + "ts", ts, tr)
              val (m, m1, m2, mz) = (l.build(tr), l.build(newTr), l.build(newTrOnlyDens), Maj().build(tr))
              //normal junto sozinho zeroR
              (accBal(m.confusion(ts)) -> accBal(m1.confusion(newTs)), accBal(m2.confusion(newTsOnlyDens)) -> accBal(mz.confusion(ts)))
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
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val (patts, (newPatts, newPattsOnlyDens)) = ds.patterns -> addAtt(dataset, ds.patterns, ds.patterns)
    ds.close()
    val nojusoze = exe(patts, preAdded = false)
    val juPreAdded = exe(newPatts, preAdded = true).filter(_._1 > -1d)
    val soPreAdded = exe(newPattsOnlyDens, preAdded = true).filter(_._1 > -1d)
    (nojusoze ++ juPreAdded ++ soPreAdded) map (x => x._1 + " " + x._2 + " ") foreach print
    println
  }

  def run() = try {
    Global.debug = argi("dbg")
    println(args.mkString(" "))
    argl("file") foreach processa(parallel = argb("par"))
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      Thread.sleep(100)
      log(e.getClass.getName + " " + e.getMessage)
  }
}
