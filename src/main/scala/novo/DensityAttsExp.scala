package novo

import clean.lib.{CM, Ds, Global}
import ml.classifiers.{Maj, RF}
import util.{Datasets, Stat}

import scala.io.Source
import scala.util.Random

object DensityAttsExp extends App with CM with DistT {
  val (rs, k) = (10, 10)
  Global.debug = -100
  val q = 20
  val parallel = args.head.contains('p')
  val datasets = Source.fromFile(args(1)).getLines().toList
  val trees = 100

//    Seq("iris", "wine", "banana") foreach processa(add = false, parallel = parallel)
  datasets foreach processa(add = false, parallel = parallel)

  //  val accs2 = datasets map processa(add = true, parallel = parallel)
  //  val accs2 = processa(add = true, parallel = parallel)("iris")

  def processa(add: Boolean, parallel: Boolean)(dataset: String) = {
    print(dataset + " ")
    val ds = Ds(dataset, readOnly = true)
    ds.open()
    val patts = /*if (add) addAtt(dataset, ds.patterns, ds.patterns) else*/ ds.patterns
    ds.close()
    val runs = (1 to rs).par map { run =>
      val shuffled = new Random(run).shuffle(patts)
      Datasets.kfoldCV(shuffled, k, parallel) { (pool, ts, fold, minSize) =>
        val step = run - 1 + "." + fold
        val seed = 1000 * run + fold
        val ((newPool, newPool2), (newTs, newTs2)) = addAtt(dataset + step + "tr", pool, pool) -> addAtt(dataset + step + "ts", ts, pool)
        //        println(step + " ok!")
        val l = RF(seed, trees)
        val (m, m1, m2, mz) = (l.build(pool), l.build(newPool), l.build(newPool2), Maj().build(pool))
        //normal junto sozinho zeroR
        (accBal(m.confusion(ts)) -> accBal(m1.confusion(newTs)), accBal(m2.confusion(newTs2)) -> accBal(mz.confusion(ts)))
      }
    }
    val (la, lb) = runs.flatten.unzip
    val (nor, jun) = la.unzip
    val (soz, zer) = lb.unzip
    Seq(nor, jun, soz, zer) foreach (x => print(Stat.media_desvioPadrao(x.toVector) + " "))
    println
  }

  val context: String = "eixos exp"
}
