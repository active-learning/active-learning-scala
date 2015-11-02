package clean.run.uti

import al.strategies.{HTUFixo, DensityWeightedTrainingUtilityFixo}
import clean.lib.Ds
import ml.classifiers.NBBatch

import scala.util.Random

object plotaTUvsMarvsATU extends App {
  val ds = Ds("mushroom", readOnly = true)
  ds.open()
  val tr = new Random(6754).shuffle(ds.patterns)
  HTUFixo(tr, NBBatch(), tr, "eucl", 1, 1, print = true).queries.toList
  //  DensityWeightedTrainingUtilityFixo(tr, NBBatch(), tr, "eucl", 1, 1, print = true).queries.toList
  ds.close()
}
