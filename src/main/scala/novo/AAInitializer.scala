package novo

import al.strategies.RandomSampling
import ml.Pattern

import scala.collection.mutable

trait AAInitializer {
  def initialSet(pool: Seq[Pattern]) = {
    val Y = pool.head.nclasses
    val set = mutable.Set[Double]()

    //apenas checa se há pelo menos um de cada classe.
    val firstof_each_class = (0 until Y).flatMap { c =>
      pool find (_.label == c)
    }.toList
    if (firstof_each_class.size < Y) Seq()
    else {
      val rnd = RandomSampling(pool)
      val qs = rnd.queries.takeWhile { p =>
        set += p.label
        set.size < Y
      }
      rnd.queries.take(qs.toList.length + 1).toList
    }
  }
}
