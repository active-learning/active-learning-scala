package novo

import al.strategies._
import clean.lib.{CM, Ds, Global}
import ml.Pattern
import ml.classifiers.{Learner, RF}
import util.Datasets

import scala.util.Random

object ALCinversionExp extends App with CM with AAInitializer{
  val (rs, k) = (2, 2)
  Global.debug = 100
  val q = 250
  val parallel = args.head.contains('p')
  val datasets = args.tail
  val trees = 25
  datasets foreach { case dataset =>
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val runs = 1 to rs map { run =>
      val shuffled = new Random(run).shuffle(ds.patterns)
      processa(ds, run, shuffled, k, parallel)
    }
    val allRuns = runs.flatten
    val sums = allRuns.map(_.flatten).reduce((a, b) => a.zip(b).map { case (x, y) => x + y })
    val divs = sums.map(_ / (rs * k).toDouble)
    divs.grouped(q).toList.transpose foreach (x => println(x.mkString(" ")))
    ds.close()
  }

  def strats(pool: Vector[Pattern], l: Learner) = List(
    Rnd(pool), Mar(l, pool), SG(l, pool, "consensus"),
    EER(l, pool, "entropy"), TU(pool, l, pool), HS(pool)
  )

  def processa(ds: Ds, run: Int, shuffled: Vector[Pattern], k: Int, parallel: Boolean) = Datasets.kfoldCV(shuffled, k, parallel) { (pool0, ts, fold, minSize) =>
    val seed = 1000 * run + fold
    val pool = new Random(seed).shuffle(pool0.sortBy(_.id))
    val labeled = initialSet(pool)
    val unlabeled = pool.diff(labeled)
    val l = RF(seed, trees)
    val res = strats(pool, l).map { s =>
      val queries = s.queries_noLabels(unlabeled, labeled).take(q - labeled.size).toList
      1 to q map { i => //seq
        val m = l.build(queries.take(i))
        accBal(m.confusion(ts))
      }
    }
    println(res)
    res
  }.toVector

  val context: String = "eixos exp"
}
