package clean.run.uti

import al.strategies.{RandomSampling, HTUFixo, DensityWeightedTrainingUtilityFixo}
import clean.lib.{CM, Ds}
import ml.classifiers._

import scala.util.Random

object plotaTUvsMarvsATU extends App with CM {
  val context: String = ""
  val ds = Ds("banana", readOnly = true)
  ds.open()
  val m = (1 to 1).par map { n =>
    val (tr, ts) = new Random(n).shuffle(ds.patterns).splitAt(1000)
    val e = KNNBatcha(5, "eucl", ds.patterns)
    val tu = DensityWeightedTrainingUtilityFixo(tr, e, tr, "eucl").queries.take(1000)
    val rnd = RandomSampling(tr).queries.take(1000)
    val l = tu.zip(rnd)
    List.fill(l.size)(l).zipWithIndex.map { case (x, i) =>
      val (t, r) = x.take(i + 1).unzip
      val (mt, mr) = e.build(t) -> e.build(r)
      acc(mt.confusion(ts)) -> acc(mr.confusion(ts))
    }
  }
  ds.close()

  val r = m.transpose.map { l =>
    val (a, b) = l.unzip
    a.sum / a.size -> b.sum / b.size
  }

  r foreach { case (a, b) => println(s"$a $b") }
}
