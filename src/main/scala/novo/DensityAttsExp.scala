package novo

import clean.lib.{CM, Ds, Global}
import ml.classifiers.RF
import util.Datasets

import scala.io.Source
import scala.util.Random

object DensityAttsExp extends App with CM with DistT {
  val (rs, k) = (5, 5)
  Global.debug = 100
  val q = 20
  val parallel = args.head.contains('p')
  val datasets = Source.fromFile(args(1)).getLines().toList
  val trees = 20
  val accs = processa(add = false, parallel = parallel)("banana")
  //  val accs = datasets map processa(add = false, parallel = parallel)
  //  val accs2 = datasets map processa(add = true, parallel = parallel)
  //  val accs2 = processa(add = true, parallel = parallel)("iris")

  def processa(add: Boolean, parallel: Boolean)(dataset: String) = {
    println(dataset)
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val patts = if (add) addAtt(dataset, ds.patterns, ds.patterns) else ds.patterns
    ds.close()
    val runs = (1 to rs).par map { run =>
      val shuffled = new Random(run).shuffle(patts)
      Datasets.kfoldCV(shuffled, k, parallel) { (pool, ts, fold, minSize) =>
        val step = run - 1 + "." + fold
        println(step + " Calculando densidades... ")
        val seed = 1000 * run + fold
        val (newPool, newTs) = addAtt(dataset + step, pool, pool) -> addAtt(dataset + step, ts, ts ++ pool)
        println(step + " ok!")
        val l = RF(seed, trees)
        val (m, m2) = l.build(pool) -> l.build(newPool)
        accBal(m.confusion(ts)) -> accBal(m2.confusion(newTs))
      }
    }
    val (l1, l2) = runs.flatten.unzip
    println(l1.sum / (rs * k) -> l2.sum / (rs * k))
  }

  val context: String = "eixos exp"
}
