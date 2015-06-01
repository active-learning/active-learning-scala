/*
 active-learning-scala: Active Learning library for Scala
 Copyright (c) 2014 Davi Pereira dos Santos

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package clean.lib

import al.strategies._
import ml.classifiers._
import ml.{Pattern, PatternParent}
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation
import org.apache.commons.math3.stat.descriptive.moment.{Kurtosis, Skewness}
import util.{Datasets, Stat}
import weka.experiment.InstanceQuerySQLite
import weka.filters.Filter

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
 * Cada instancia desta classe representa um ML dataset.
 */
case class Ds(dataset: String, readOnly: Boolean) extends Db(s"$dataset", readOnly) with Blob with CM with EntropyMeasure with FilterTrait with LearnerTrait {
  lazy val bestPassiveLearner = {
    lazy val learners = Seq(
      KNNBatcha(5, "eucl", Seq(), weighted = true)
      , C45()
      , RF()
      , NBBatch()
      , CIELMBatch()
      , SVMLibRBF()
    )
    learners.map { l =>
      val vs = for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield Kappa(this, Passive(Seq()), l, r, f)(-1).read(this).getOrElse(quit("Kappa passiva não encontrada"))
      l -> Stat.media_desvioPadrao(vs.toVector)._1
    }.maxBy(_._2)._1
  }
  override lazy val toString = dataset
  override val context = dataset
  lazy val patterns = fetchPatterns("i order by id asc")
  lazy val n = patterns.size
  lazy val nclasses = patterns.head.nclasses
  lazy val nattributes = patterns.head.nattributes
  //  lazy val Q = {
  //    val r = fetchQ()
  //    if (r.isEmpty) error("Q not found.") else r.head.toInt
  //  }
  lazy val poolSize = {
    val Us = expectedPoolSizes(Global.folds)
    Us.sum / Us.size.toDouble
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val poolSizeByNatts = poolSize / nattributes
  //  lazy val QbyUavg = Q / Uavg
  lazy val nomCount = patterns.head.enumerateAttributes().count(_.isNominal)
  lazy val numCount = patterns.head.enumerateAttributes().count(_.isNumeric)
  lazy val nomByNum = if (numCount == 0) nomCount else nomCount / numCount.toDouble
  lazy val hist = patterns.groupBy(_.label).toList.sortBy(_._1).map(_._2.size / n.toDouble).toArray
  lazy val minority = 100 * hist.min
  lazy val majority = 100 * hist.max
  lazy val description = List[Double](poolSize, nclasses, nattributes, nomCount).map(x => x.round.toInt) -> List(majority, minority, normalized_entropy(hist))
  lazy val description2 = List[Double](poolSize.round.toInt, nattributes, nomCount, majority.round.toInt).map(x => x.toInt) -> List(majority, minority, normalized_entropy(hist))
  lazy val metaAttsHumanAndKnowingLabels = List[Double](nclasses, nattributes, poolSize, poolSizeByNatts, 100d * nomCount / nattributes, majority, minority, majority / minority, 0d)
  //   lazy val metaAttsHumanAndKnowingLabels = List[Double](0, 0, 0, 0, 0, 0, 0, 0, 0d)
  //normalized_entropy(hist))
  lazy val nominalAtts = patterns.head.enumerateAttributes().toList.dropRight(1).filter(_.isNominal).map(_.name())
  lazy val numericAtts = patterns.head.enumerateAttributes().toList.filter(_.isNumeric).map(_.name())
  lazy val nominalDistinctValues = if (nominalAtts.isEmpty) List(Array("")) else readString(s"select ${nominalAtts.mkString(",")} from i").transpose.map(_.toArray)
  lazy val nominalDistinctCount = nominalDistinctValues.map(_.distinct.size)
  lazy val nominalDistinctCountAvg = nominalDistinctCount.sum / nominalDistinctCount.size
  lazy val numericValues = if (numericAtts.isEmpty) List(Array(0d)) else read(s"select ${numericAtts.mkString(",")} from i").transpose.map(_.toArray)
  lazy val (medias, desvios) = numericValues.map(x => Stat.media_desvioPadrao(x.toVector)).unzip
  lazy val entropias = if (numericAtts.isEmpty) List(0d)
  else numericValues map { x =>
    val tmp = normalized_entropy(x.map(_ / n))
    if (tmp.isNaN) 0d else tmp
  }
  lazy val skewnesses = if (numericValues.map(_.toList).sameElements(List(List(0d)))) List(0d)
  else numericValues map { x =>
    val tmp = new Skewness().evaluate(x)
    if (tmp.isNaN) {
      println(s"skew NaN: ${x.toList} \n${patterns.head.enumerateAttributes().toList} \n${numericValues.map(_.toList)}")
      sys.exit(0)
    }
    tmp
  }
  lazy val kurtoses = if (numericValues.map(_.toList).sameElements(List(List(0d)))) List(0d)
  else numericValues map { x =>
    val tmp = new Kurtosis().evaluate(x)
    if (tmp.isNaN) {
      println(s"kurt NaN: ${x.toList} \n${patterns.head.enumerateAttributes().toList}")
      sys.exit(0)
    }
    tmp
  }

  lazy val correls = if (numericValues.size < 2) List(0d) else for (a1 <- numericValues; a2 <- numericValues) yield new PearsonsCorrelation().correlation(a1, a2)

  lazy val correleucmah = {
    0d
  }
  lazy val correleucman = {
    0d
  }
  lazy val correlmanmah = {
    0d
  }

  //   val subnonHumanNumAttsNames = "\"#classes\",\"#exemplos/#atributos\",\"%nominais\""
  val humanNumAttsNames = "\"\\\\#classes\",\"\\\\#atributos\",\"\\\\#exemplos\",\"$\\\\frac{\\\\#exemplos}{\\\\#atrib.}$\",\"\\\\%nominais\",\"\\\\%major.\",\"\\\\%minor.\",\"$\\\\frac{\\\\%major.}{\\\\%minor.}$\",\"entropia da distr. de classes\""
  val descriptionNames = Seq( """$|\mathcal{U}|$""", """$|Y|$""", "atributos", "nominais", """\makecell{majoritária\\(\%)}""", """\makecell{minoritária\\(\%)}""", """\makecell{entropia da \\distr. de classes}""")

  lazy val submetaAtts = List[Double](nclasses, poolSizeByNatts, 100d * nomCount / nattributes)
  lazy val nonHumanNumAttsNames = ("\"#classes\",\"#atributos\",\"#exemplos\"," +
    "\"#exemplos/#atributos\",\"%nominais\",\"log(#exs)\",\"log(#exs/#atrs)\"," +
    "skewnessesmin,skewavg,skewnessesmax,skewnessesminByskewnessesmax," +
    "kurtosesmin,kurtavg,kurtosesmax,kurtosesminBykurtosesmax," +
    "nominalValuesCountmin,nominalValuesCountAvg,nominalValuesCountmax,nominalValuesCountminBynominalValuesCountmax," +
    "mediasmin,mediasavg,mediasmax,mediasminBymediasmax," +
    "desviosmin,desviosavg,desviosmax,desviosminBydesviosmax," +
    "entropiasmin,entropiasavg,entropiasmax,entropiasminByentropiasmax," +
    "correlsmin,correlsavg,correlsmax,correlsminBycorrelsmax,correleucmah,correleucman,correlmanmah").split(",")
  lazy val metaAttsrfmap = mutable.Map[(Int, Int), Seq[(String, Double, String)]]()

  lazy val metaAtts = {
    val r = List[Double](
      nclasses, nattributes, poolSize,
      poolSizeByNatts, 100d * nomCount / nattributes, math.log10(poolSize), math.log10(poolSizeByNatts),
      skewnesses.min, skewavg, skewnesses.max, skewnesses.min / skewnesses.max,
      kurtoses.min, kurtavg, kurtoses.max, kurtoses.min / kurtoses.max,
      nominalDistinctCount.min, nominalDistinctCountAvg, nominalDistinctCount.max, nominalDistinctCount.min / nominalDistinctCount.max,
      medias.min, mediasavg, medias.max, medias.min / medias.max,
      desvios.min, desviosavg, desvios.max, desvios.min / desvios.max,
      entropias.min, entropiasavg, entropias.max, entropias.min / entropias.max,
      correls.min, correlsavg, correls.max, correls.min / correls.max, correleucmah, correleucman, correlmanmah)
    nonHumanNumAttsNames zip r map (x => (x._1, x._2, "numeric"))
  }
  lazy val mediasavg = medias.sum / medias.size
  lazy val desviosavg = desvios.sum / desvios.size
  lazy val entropiasavg = entropias.sum / entropias.size
  lazy val skewavg = skewnesses.sum / skewnesses.size
  lazy val kurtavg = kurtoses.sum / kurtoses.size
  lazy val correlsavg = correls.sum / correls.size

  def metaAttsrf(r: Int, f: Int) = metaAttsrfmap getOrElseUpdate((r, f), {
    val res = List[Double](
      nclasses, nattributes, poolSize,
      poolSizeByNatts, 100d * nomCount / nattributes, math.log10(poolSize), math.log10(poolSizeByNatts),
      skewnessesrf(r, f).min, skewavgrf(r, f), skewnessesrf(r, f).max, skewnessesrf(r, f).min / skewnessesrf(r, f).max,
      kurtosesrf(r, f).min, kurtavgrf(r, f), kurtosesrf(r, f).max, kurtosesrf(r, f).min / kurtosesrf(r, f).max,
      nominalDistinctCount.min, nominalDistinctCountAvg, nominalDistinctCount.max, nominalDistinctCount.min / nominalDistinctCount.max,
      mediasrf(r, f).min, mediasavgrf(r, f), mediasrf(r, f).max, mediasrf(r, f).min / mediasrf(r, f).max,
      desviosrf(r, f).min, desviosavgrf(r, f), desviosrf(r, f).max, desviosrf(r, f).min / desviosrf(r, f).max,
      entropiasrf(r, f).min, entropiasavgrf(r, f), entropiasrf(r, f).max, entropiasrf(r, f).min / entropiasrf(r, f).max,
      correlsrf(r, f).min, correlsavgrf(r, f), correlsrf(r, f).max, correlsrf(r, f).min / correlsrf(r, f).max, correleucmah, correleucman, correlmanmah)
    val res2 = res.map { case Double.NaN => 0; case x => x }
    nonHumanNumAttsNames zip res2 map (x => (x._1, x._2, "numeric"))
  })

  def mediasavgrf(r: Int, f: Int) = mediasrf(r, f).sum / mediasrf(r, f).size

  def desviosavgrf(r: Int, f: Int) = desviosrf(r, f).sum / desviosrf(r, f).size

  def entropiasavgrf(r: Int, f: Int) = entropiasrf(r, f).sum / entropiasrf(r, f).size

  def skewavgrf(r: Int, f: Int) = skewnessesrf(r, f).sum / skewnessesrf(r, f).size

  def kurtavgrf(r: Int, f: Int) = kurtosesrf(r, f).sum / kurtosesrf(r, f).size

  def correlsavgrf(r: Int, f: Int) = correlsrf(r, f).sum / correlsrf(r, f).size


  /**
   * 3 ultimos metaatts não implementados, e nem lembro o que seria
   * falta implementar algo que considere o balanceamento dentro de cada atributo nominal
   * @param r
   * @param f
   * @return
   */

  def kurtosesrf(r: Int, f: Int) = if (numericValuesrf(r, f).map(_.toList).sameElements(List(List(0d)))) List(0d)
  else numericValuesrf(r, f) map { x =>
    if (x.forall(_ == x.head)) 0d
    else {
      val tmp = new Kurtosis().evaluate(x)
      if (tmp.isNaN) {
        println(s"kurt NaN: ${x.toList} \n${patternsrf(r, f).head.enumerateAttributes().toList}")
        sys.exit(0)
      }
      tmp
    }
  }

  lazy val correlsrfmap = mutable.Map[(Int, Int), List[Double]]()

  def correlsrf(r: Int, f: Int) = correlsrfmap getOrElseUpdate((r, f), if (numericValuesrf(r, f).size < 2) List(0d) else for (a1 <- numericValuesrf(r, f); a2 <- numericValuesrf(r, f)) yield new PearsonsCorrelation().correlation(a1, a2))

  lazy val entropiasrfmap = mutable.Map[(Int, Int), List[Double]]()

  def entropiasrf(r: Int, f: Int) = entropiasrfmap getOrElseUpdate((r, f), if (numericAtts.isEmpty) List(0d)
  else numericValuesrf(r, f) map { x =>
    val tmp = normalized_entropy(x.map(_ / n))
    if (tmp.isNaN) 0d else tmp
  })

  def mediasrf(r: Int, f: Int) = numericValuesrf(r, f).map(x => Stat.media_desvioPadrao(x.toVector)._1)


  lazy val skewnessesrfmap = mutable.Map[(Int, Int), List[Double]]()

  def desviosrf(r: Int, f: Int) = numericValuesrf(r, f).map(x => Stat.media_desvioPadrao(x.toVector)._2)

  def skewnessesrf(r: Int, f: Int) = skewnessesrfmap getOrElseUpdate((r, f), if (numericValuesrf(r, f).map(_.toList).sameElements(List(List(0d)))) List(0d)
  else numericValuesrf(r, f) map { x =>
    if (x.forall(_ == x.head)) 0d
    else {
      val tmp = new Skewness().evaluate(x)
      if (tmp.isNaN) {
        println(s"skew NaN: ${x.toList} \n${patternsrf(r, f).head.enumerateAttributes().toList} \n${numericValuesrf(r, f).map(_.toList.mkString(" ")).mkString("\n")}")
        sys.exit(0)
      } else tmp
    }
  })

  lazy val numericValuesrfmap = mutable.Map[(Int, Int), List[Array[Double]]]()
  lazy val allAtts = patterns.head.enumerateAttributes().toList.dropRight(1)

  def numericValuesrf(r: Int, f: Int) = numericValuesrfmap.getOrElseUpdate((r, f), if (numericAtts.isEmpty) List(Array(0d))
  else (for (p <- patternsrf(r, f)) yield p.vector.zip(allAtts).filter(_._2.isNumeric).map(_._1).toArray).toList).transpose.map(_.toArray)

  def patternsrf(r: Int, f: Int) = getPool(r, f)

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //  lazy val maj = read("select count(1) from i group by c").map(_.head).sorted.last / n
  lazy val attsFromRNames = Seq("AH-conect.-Y", "AH-Dunn-Y", "AH-silhueta-Y", "AH-conect.-1.5Y", "AH-Dunn-1.5Y", "AH-silhueta-1.5Y",
    "AH-conect.-2Y", "AH-Dunn-2Y", "AH-silhueta-2Y", "kM-conect.-Y", "kM-Dunn-Y", "kM-silhueta-Y", "kM-conect.-1.5Y", "kM-Dunn-1.5Y",
    "kM-silhueta-1.5Y", "kM-conect.-2Y", "kM-Dunn-2Y", "kM-silhueta-2Y").map(x => "\"" + x + "\"")

  def metaAttsFromR(r: Int, f: Int) = {
    val s = Source.fromFile(s"/home/davi/wcs/als/csv/$this-r$r-f$f-normalized-pool.arff.csv")
    val res = s.getLines().toList.last.split(",").map(_.toDouble)
    s.close()
    attsFromRNames zip res map (x => (x._1, x._2, "numeric"))
  }

  def isFinishedMea(stratsLeas: String) = readString(s"select finished from mea") match {
    case lista if lista.map(_.head).contains(stratsLeas) => true
    case x => false
  }

  def markAsFinishedMea(stratsLeas: String): Unit = {
    write(s"insert into mea values ('$stratsLeas')")
  }

  def isFinishedRun(stratsLeas: String) = readString(s"select finished from run") match {
    case lista if lista.map(_.head).contains(stratsLeas) => true
    case x => false
  }

  def markAsFinishedRun(stratsLeas: String): Unit = {
    write(s"insert into run values ('$stratsLeas')")
  }

  //  def passiveAcc(learner: Learner, r: Int, f: Int) = {
  //    if (learner.id > 3) {
  //      error(s"$learner doesn't !")
  //      if (learner.id > 5) error(s"$learner needs filter; filtered passAcc not implemented!")
  //      val shuffled = new Random(r).shuffle(patterns)
  //      val ps = Datasets.kfoldCV(shuffled, Global.folds, parallel = true) { (tr, ts, f, m) => f -> (tr -> ts)}.toMap
  //      learner.build(ps(f)._1).accuracy(ps(f)._2)
  //    }
  //    val cm = getCMs(RandomSampling(Seq()), learner, r, f).last._2
  //    acc(cm)
  //  }

  //  private def fetchQ() = read(s"select v from r where m=0 AND p=-1").map(_.head)

  /**
   * not exact because of fixed learner strats.
   * @param strats
   * @param learners
   * @return
   */
  def progress(strats: Seq[Int], learners: Seq[Int]) = read(s"select count(0) from r,p where p.id=p and p.s in (${
    strats.mkString(",")
  }) and p.l in (${
    learners.mkString(",")
  })").head.head / (strats.size * learners.size * Global.runs * Global.folds)

  /**
   * Reads all SQLite patterns according to given SQL conditions.
   * Note that nominal attributes must have all possible values present in the resulting sample!
   * It opens a new connection, so it will have to wait to open a connection under writing ops.
   */
  private def fetchPatterns(sqlTail: String) = try {
    val ids = read("select i.id from " + sqlTail).map(_.head.toInt)
    log(s"Fetching patterns...")
    val query = new InstanceQuerySQLite()
    val url = s"jdbc:mysql://${
      Global.mysqlHost(readOnly = true)
    }:${
      Global.mysqlPort(readOnly = true)
    }/" + database
    query.setDatabaseURL(url)
    query.setUsername("davi")
    query.setPassword(Global.mysqlPass(readOnly = true))
    query.setQuery("select i.* from " + sqlTail)
    query.setDebug(false)
    val instances = query.retrieveInstances()
    instances.setClassIndex(instances.numAttributes() - 1)
    instances.setRelationName(dataset)
    val parent = PatternParent(instances)
    val res = instances.zip(ids).map {
      case (instance, idx) => Pattern(idx, instance, missed = false, parent)
    }
    query.close()
    res.toVector
  } catch {
    case ex: Exception => error(s"${
      ex.getStackTraceString
    } \n Problems reading dataset $database: ${
      ex.getMessage
    }")
  }

  def poolId(strat: Strategy, learner: Learner, run: Int, fold: Int): Option[Int] = poolId(strat.id, learner.id, run, fold)

  private def poolId(idStrat: Int, lid: Int, run: Int, fold: Int) =
    read(s"SELECT id FROM p WHERE s=$idStrat and l=$lid and r=$run and f=$fold") match {
      case List() => None
      case List(seq) => Some(seq.head.toInt)
    }

  //  def isQCalculated = fetchQ().nonEmpty

  def areQueriesFinished(poolSize: Int, strat: Strategy, run: Int, fold: Int, binaf: Filter, zscof: Filter, completeIt: Boolean, expectedAmount0: Int): Boolean = if (readOnly) error("read only")
  else {
    val expectedAmount = math.min(poolSize, expectedAmount0)
    val (sid, lid) = (strat.id, strat.learner.id)
    poolId(sid, lid, run, fold) match {
      case None =>
        log(s"No queries: no pid found for $sid/$lid $run.$fold .")
        false
      case Some(pid) =>
        val PoolSize = poolSize
        val (qs, lastT) = read(s"SELECT COUNT(1),max(t+0) FROM q WHERE p=$pid") match {
          case List(Vector(c, m)) => c.toInt -> m.toInt
          case List() => error(s"Inconsistency: there is a pool $pid for no queries!")
        }
        if (qs != lastT + 1) error(s"Inconsistency: $qs queries differs from last timeStep+1 ${
          lastT + 1
        }")
        sid match {
          case x if x == 0 => qs match {
            case 0 => error(s"Inconsistency: there is a pool $pid for no queries! l:$lid")
            case PoolSize => true
            case _ => error(s"$qs previous rnd queries should be $PoolSize.  l:$lid")
          }
          case _ => qs match {
            case 0 => error(s"Inconsistency: there is a pool $pid for no queries!  l:$lid  s:$sid")
            case q if q >= expectedAmount => true
            case q => if (completeIt) {
              log(s"$qs previous $q queries should be at least $expectedAmount. s:$strat l:${
                strat.learner
              }. Completing it...", 20)
              val qrs = queries(strat, run, fold, binaf, zscof)
              val newqrs = strat.resume_queries(qrs).take(expectedAmount - q)
              if (newqrs.size + q != expectedAmount) quit(s"wrong new total of queries: ${
                newqrs.size + q
              }")
              val sqls = newqrs.zipWithIndex map {
                case (patt, t0) =>
                  val t = t0 + lastT + 1
                  s"INSERT INTO q values ($pid, $t, ${
                    patt.id
                  })"
              }
              batchWrite(sqls.toList)
              sqls.size + q == expectedAmount
            } else {
              log(s"$qs previous $q queries should be at least $expectedAmount. s:$strat l:${
                strat.learner
              }. |U|=$poolSize. But not allowed to complete it...", 20)
              false
            }
          }
        }
    }
  }

  def areHitsFinished(poolSize: Int, testSet: Seq[Pattern], strat: Strategy, learner: Learner, run: Int, fold: Int, binaf: Filter, zscof: Filter, completeIt: Boolean, expectedAmount0: Int) = if (readOnly) error("read only")
  else {
    val ExpectedHitsForFullPool = poolSize - nclasses + 1
    val expectedAmount = math.min(ExpectedHitsForFullPool, expectedAmount0)
    if (learner.id != strat.learner.id && strat.id > 1 && !Global.agnosticasa.contains(strat.id)) error(s"areHitsFinished: Provided learner $learner is different from gnostic strategy's learner $strat.${
      strat.learner
    }")
    else if (!areQueriesFinished(poolSize, strat, run, fold, null, null, completeIt = false, expectedAmount + nclasses - 1)) error(s"Queries must be finished to check hits! |U|=$poolSize")
    else
      poolId(strat, learner, run, fold) match {
        case None => false
        case Some(pid) =>
          lazy val ExpectedHitsForNormalPool = expectedAmount
          val hs = (read(s"SELECT COUNT(1),max(t+0) FROM h WHERE p=$pid") match {
            case List(Vector(0)) | List(Vector(0, 0)) => 0d
            case List(Vector(c, m)) => if (c == m - nclasses + 2) c
            else error(s"Inconsistency: $c cms differs from last timeStep+1 = ${
              m - nclasses + 2
            }")
            case _ => error(s"Inconsistency: there is a pool $pid for no hits! s=$strat l=$learner")
          }).toInt

          def completa() = {
            log(s"$hs previous rnd hits should be at least $ExpectedHitsForNormalPool.\n ExpectedHitsForFullPool:$ExpectedHitsForFullPool s=$strat l=$learner. Completing...")
            //gera hits e sql strs
            val usedQueries = hs + nclasses - 1
            val lastUsedT = usedQueries - 1
            val (usedPatterns, rest) = queries(strat, run, fold, binaf, zscof).take(expectedAmount + nclasses - 1).splitAt(usedQueries)
            if (usedPatterns.size != usedQueries) error("Problems taking hit-used queries.")
            var m = learner.build(usedPatterns)
            val tuples = rest.zipWithIndex.map {
              case (patt, idx) =>
                val t = idx + lastUsedT + 1
                m = learner.update(m, fast_mutable = true)(patt)
                val cm = m.confusion(testSet)
                val blob = confusionToBlob(cm)
                (s"INSERT INTO h values ($pid, $t, ?)", blob)
            }.toList
            val (sqls, blobs) = tuples.unzip
            log(tuples.mkString("\n"), 20)
            batchWriteBlob(sqls, blobs)
            sqls.size + hs == expectedAmount
          }

          //               println(s"lixo================================")
          (strat.id, learner.id) match {
            case (s, l) if s == 0 && l < 4 => hs match {
              case 0 => error(s"Inconsistency: there is a pool $pid for no hits!")
              case ExpectedHitsForFullPool => true
              case _ => error(s"$hs previous rnd hits should be $ExpectedHitsForFullPool")
            }
            case (s, l) if s == 0 => hs match {
              case 0 => error(s"Inconsistency: there is a pool $pid for no hits!")
              case nhs if nhs >= ExpectedHitsForNormalPool => true
              case nhs => if (completeIt) completa()
              else {
                log(s"$hs previous rnd hits should be at least $ExpectedHitsForNormalPool.\n ExpectedHitsForFullPool:$ExpectedHitsForFullPool s=$strat l=$learner. But not allowed to complete ...", 20)
                false
              }
            }
            case (s, l) if s > 0 => hs match {
              case 0 => false
              case nhs if nhs >= ExpectedHitsForNormalPool => true
              case nhs => if (completeIt) completa()
              else {
                log(s"$hs previous rnd hits should be at least $ExpectedHitsForNormalPool.\n ExpectedHitsForFullPool:$ExpectedHitsForFullPool s=$strat l=$learner. But not allowed to complete ...", 20)
                false
              }
            }
          }
      }
  }

  def countQueries(strat: Strategy, run: Int, fold: Int) = {
    val pid = poolId(strat, strat.learner, run, fold).getOrElse(quit(s"Pool not found for  s=${
      strat.id
    } and l=${
      strat.learner.id
    } and r=$run and f=$fold !"))
    read(s"SELECT COUNT(1) FROM q WHERE p=$pid") match {
      case List() => 0
      case List(seq) => seq.head.toInt
    }
  }

  /**
   * Get the list of all CMs sorted by time.
   * @return
   */
  def getCMs(pid: Int)(ti: Int, tf: Int) = {
    val cms = mutable.LinkedHashMap[Int, Array[Array[Int]]]()
    readBlobs(s"select mat,t from h WHERE p=$pid and t>=$ti and t <=$tf ORDER BY t") foreach {
      case (b, t) => cms += t -> blobToConfusion(b, nclasses)
    }
    cms
  }

  /**
   * Retrieve instances from disk in querying order.
   * Apply filters previously calibrated with training set.
   * Fetch all instances first to avoid missing nominal attribute values in dataset/"ARFF" header.
   * @param strat
   * @param run
   * @param fold
   * @param binaf
   * @param zscof
   * @return
   */
  def queries(strat: Strategy, run: Int, fold: Int, binaf: Filter, zscof: Filter): List[Pattern] = queries(strat.id, strat.learner.id, run, fold, binaf, zscof)

  def queries(stratid: Int, lid: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    val patts = read(s"SELECT i.id FROM i, q, p where i.id=q.i and p.id=p and p.s=$stratid and p.l=$lid and p.r=$run and p.f=$fold order by t asc") match {
      case List() => error("No queries found!" +(stratid, lid, run, fold))
      case list: List[Vector[Double]] =>
        val ids = list.map(_.head.toInt)
        val m = (patterns map (p => p.id -> p)).toMap
        ids map m
    }
    if (binaf == null) patts
    else {
      val binaPatts = Datasets.applyFilter(binaf)(patts)
      Datasets.applyFilter(zscof)(binaPatts).toList
    }
  }

  def writeQueries(strat: Strategy, run: Int, fold: Int, q: Int) = if (readOnly) error("read only")
  else {
    poolId(strat, strat.learner, run, fold) match {
      case Some(queryPoolId) => quit(s"Pool $run.$fold já estava gravado para $strat.${
        strat.learner
      } referente às queries a gravar.")
      case None =>
        val qs = strat.queries.take(q).toList
        val poolSQL = s"INSERT INTO p VALUES (NULL, ${
          strat.id
        }, ${
          strat.learner.id
        }, $run, $fold)"
        val sqls = poolSQL +: (qs.zipWithIndex map {
          case (patt, t) =>
            s"INSERT INTO q select id, $t, ${
              patt.id
            } from p where s=${
              strat.id
            } and l=${
              strat.learner.id
            } and r=$run and f=$fold"
        })
        batchWrite(sqls.toList)
        qs
    }
  }

  /**
   * Valor de Time step das tabelas q e h é o mesmo.
   * Primeira conf mat é referente à mat. de conf. do primeiro build, i.e. t = |Y| - 1.
   * Outra forma de ver é que as primeiras |Y| queries geram a primeira mat. de conf.
   * Strat.learner deve ser igual a learner, em strats gnósticas.
   * @param testSet
   * @param queries
   * @param strat
   * @param run
   * @param fold
   * @param learner
   * @return
   */
  def writeHits(poolSize: Int, testSet: Seq[Pattern], queries: Vector[Pattern], strat: Strategy, run: Int, fold: Int, h: Int)(learner: Learner) = if (readOnly) error("read only")
  else if (learner.id != strat.learner.id && strat.id > 1 && !Global.agnosticasa.contains(strat.id))
    error(s"Provided learner $learner is different from gnostic strategy's learner $strat.${
      strat.learner
    }")
  else {
    //Apenas agnostic strats gravam um poolId que tem NoLearner, não-reutilizável pra hits.
    val insertIntoP = poolId(strat, learner, run, fold) match {
      case Some(pid) => if (strat.id < 2) quit(s"Pool $run.$fold já estava gravado para $strat.$learner referente aos hits de $strat.") else "SELECT 1"
      //         case Some(pid) => if (strat.id < 2 || Global.agnosticasa.dropRight(83).contains(strat.id)) quit(s"Pool $run.$fold já estava gravado para $strat.$learner referente aos hits de $strat.") else "SELECT 1"
      case None =>
        if (strat.id < 2 || Global.agnosticasa.contains(strat.id)) s"INSERT INTO p VALUES (NULL, ${
          strat.id
        }, ${
          learner.id
        }, $run, $fold)"
        else quit(s"Missing gnostic queries pid for hits.")
    }

    //para rnd e quaisquer learners, |queries| = |U|.
    val expectedQ = if (strat.id == 0) poolSize else h + nclasses - 1
    if (expectedQ > queries.size) quit(s"Number of ${
      queries.size
    } provided queries for hits is lesser than $expectedQ expected!")

    //para rnd com learners especiais Q is not yet defined, pega |U|; senão pega apenas Q das queries fornecidas
    val qtdQueriesToTake = if (strat.id == 0 && learner.id < 4) poolSize else h + nclasses - 1
    val (initialPatterns, rest) = queries.take(qtdQueriesToTake).splitAt(nclasses)
    if (nclasses != initialPatterns.size || initialPatterns.size + rest.size != qtdQueriesToTake) error("Problems picking initialPatterns for hits.")

    //gera hits e sql strs
    var m = learner.build(initialPatterns)
    val tuples = (insertIntoP, null) +: ((null +: rest).zipWithIndex map {
      case (patt, idx) =>
        val t = idx + nclasses - 1
        if (patt != null) m = learner.update(m, fast_mutable = true)(patt)
        val cm = m.confusion(testSet)
        val blob = confusionToBlob(cm)
        (s"INSERT INTO h SELECT id, $t, ? FROM p where s=${
          strat.id
        } and l=${
          learner.id
        } and r=$run and f=$fold", blob)
    }).toList
    val (sqls, blobs) = tuples.unzip
    log(tuples.mkString("\n"), 20)
    batchWriteBlob(sqls, blobs)
  }

  def expectedPoolSizes(folds: Int) = {
    val parteIgual = n / folds
    val resto = n % folds
    val foldSizes = Array.fill(folds)(parteIgual)
    0 until resto foreach (i => foldSizes(i) += 1)
    foldSizes map (n - _)
  }

  def pids(sid: Int, lid: Int) = read(s"SELECT id FROM p WHERE s=$sid and l=$lid") match {
    case List() => None
    case l => Some(l.map(_.head.toInt))
  }

  lazy val poolMap = mutable.Map[(Int, Int), Seq[Pattern]]()

  def getPool(r: Int, f: Int) = poolMap getOrElseUpdate((r, f), queries(0, 0, r, f, null, null))

  def suavidade(l: Learner) = {
    val le = allLearners(patterns, 42).find(_.id == l.id).getOrElse(quit("suavidade problems"))
    val ts = new Random(0).shuffle(patterns).take(15 * nclasses)
    val (fts, binaf, zscof) = criaFiltro(patterns, 0)
    val tr = queries(0, 0, 0, 0, null, null).take(nclasses)
    lazy val ftr = aplicaFiltro(tr, 0, binaf, zscof)
    val tr2 = if (Seq(6, 8, 11).contains(l.id)) ftr else tr
    val ts2 = if (Seq(6, 8, 11).contains(l.id)) fts else ts
    le.build(tr2).predictionEntropy(ts2)._1
  }
}