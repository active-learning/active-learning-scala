package novo

import al.strategies.RandomSampling
import clean.lib.Ds
import ml.Pattern

import scala.collection.mutable

trait AAInitializer {
  def initialSet(pool: Seq[Pattern]) = {
    val Y = pool.head.nclasses
    val set = mutable.Set[Double]()
    val rnd = RandomSampling(pool)
    val qs = rnd.queries.takeWhile { p =>
      set += p.label
      set.size < Y
    }
    rnd.queries.take(qs.toList.length + 1).toList
  }
}
