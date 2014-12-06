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

package clean

import al.strategies._
import clean.res.Measure
import ml.classifiers.{Learner, NoLearner}
import ml.{Pattern, PatternParent}
import util.Datasets
import weka.experiment.InstanceQuerySQLite
import weka.filters.Filter

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Cada instancia desta classe representa um ML dataset.
 */
case class Ds(dataset: String, readOnly: Boolean) extends Db(s"$dataset", readOnly) with Blob with CM {
   def isFinished(Budget: Int) = read(s"select b from f") match {
      case List(Vector(Budget)) => true
      case _ => false
   }

   def markAsFinished(budget: Int): Unit = {
      write(s"insert into f values ($budget)")
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
   lazy val Uavg = {
      val Us = expectedPoolSizes(5)
      Us.sum / Us.size.toDouble
   }
   lazy val nattsByUavg = nattributes / Uavg
   //  lazy val QbyUavg = Q / Uavg
   lazy val nomCount = patterns.head.enumerateAttributes().count(_.isNominal)
   lazy val numCount = patterns.head.enumerateAttributes().count(_.isNumeric)
   lazy val nomByNum = if (numCount == 0) nomCount else nomCount / numCount.toDouble
   lazy val metaAtts = List[Double](nclasses, nattributes, Uavg, nattsByUavg, nomCount, numCount, nomByNum)

   //  lazy val maj = read("select count(1) from i group by c").map(_.head).sorted.last / n

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
   def progress(strats: Seq[Int], learners: Seq[Int]) = read(s"select count(0) from r,p where p.id=p and p.s in (${strats.mkString(",")}) and p.l in (${learners.mkString(",")})").head.head / (strats.size * learners.size * Global.runs * Global.folds)

   def reset() {
      ???
      //    //query [pool timeStep instance]
      //    //hit [pool timeStep blobMatrix(realClass X guessedClass values)] (confusion matrix blob)
      //    //pool [strat learner run fold]
      //    //result [app.measure pool value] (Q, ...)
      //    //time [pool value] in seconds
      //    write("drop table if exists q")
      //    write("drop table if exists h")
      //    write("drop table if exists p")
      //    write("drop table if exists r")
      //    write("drop table if exists t")
      //    write("CREATE TABLE q ( p INT, t INT, i INT, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, UNIQUE (p, i) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id), FOREIGN KEY (i) REFERENCES i (id) ); ")
      //    write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) );")
      //    write("CREATE TABLE p ( id INTEGER PRIMARY KEY ON CONFLICT ROLLBACK, s INT, l INT, r INT, f INT, UNIQUE (s, l, r, f) ON CONFLICT ROLLBACK ); ")
      //    write("CREATE TABLE r ( m INT, p INT, v FLOAT, PRIMARY KEY (m, p) ON CONFLICT ROLLBACK, FOREIGN KEY (m) REFERENCES measure (id), FOREIGN KEY (p) REFERENCES p (id) ); ")
      //    write("CREATE TABLE f ( b INT, PRIMARY KEY (b) ); ")
      //    //    write("CREATE TABLE t ( p INTEGER PRIMARY KEY ON CONFLICT ROLLBACK, v INT, FOREIGN KEY (p) REFERENCES p (id) ); ")
   }

   /**
    * Reads all SQLite patterns according to given SQL conditions.
    * Note that nominal attributes must have all possible values present in the resulting sample!
    * It opens a new connection, so it will have to wait to open a connection under writing ops.
    */
   private def fetchPatterns(sqlTail: String) = try {
      val ids = read("select i.id from " + sqlTail).map(_.head.toInt)
      log(s"Fetching patterns...")
      val query = new InstanceQuerySQLite()
      val url = s"jdbc:mysql://${Global.mysqlHost(readOnly = true)}:${Global.mysqlPort(readOnly = true)}/" + database
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

   private def poolId(strat: Strategy, learner: Learner, run: Int, fold: Int): Option[Int] = poolId(strat.id, learner.id, run, fold)

   private def poolId(idStrat: Int, lid: Int, run: Int, fold: Int) =
      read(s"SELECT id FROM p WHERE s=$idStrat and l=$lid and r=$run and f=$fold") match {
         case List() => None
         case List(seq) => Some(seq.head.toInt)
      }

   //  def isQCalculated = fetchQ().nonEmpty

   def areQueriesFinished(poolSize: Int, strat: Strategy, run: Int, fold: Int, binaf: Filter, zscof: Filter, completeIt: Boolean, expectedAmount0: Int): Boolean = {
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
            if (qs != lastT + 1) error(s"Inconsistency: $qs queries differs from last timeStep+1 ${lastT + 1}")
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
                     log(s"$qs previous $q queries should be at least $expectedAmount. s:$strat l:${strat.learner}. Completing it...", 20)
                     val qrs = queries(strat, run, fold, binaf, zscof)
                     val newqrs = strat.resume_queries(qrs).take(expectedAmount - q)
                     if (newqrs.size + q != expectedAmount) quit(s"wrong new total of queries: ${newqrs.size + q}")
                     val sqls = newqrs.zipWithIndex map { case (patt, t0) =>
                        val t = t0 + lastT + 1
                        s"INSERT INTO q values ($pid, $t, ${patt.id})"
                     }
                     batchWrite(sqls.toList)
                     sqls.size + q == expectedAmount
                  } else {
                     log(s"$qs previous $q queries should be at least $expectedAmount. s:$strat l:${strat.learner}. |U|=$poolSize. But not allowed to complete it...", 20)
                     false
                  }
               }
            }
      }
   }

   def areHitsFinished(poolSize: Int, testSet: Seq[Pattern], strat: Strategy, learner: Learner, run: Int, fold: Int, binaf: Filter, zscof: Filter, completeIt: Boolean, expectedAmount0: Int) = {
      val ExpectedHitsForFullPool = poolSize - nclasses + 1
      val expectedAmount = math.min(ExpectedHitsForFullPool, expectedAmount0)
      if (learner.id != strat.learner.id && strat.id > 1 && !Seq(211, 601, 361, 66361, 901, 391, 66391).contains(strat.id)) error(s"areHitsFinished: Provided learner $learner is different from gnostic strategy's learner $strat.${strat.learner}")
      else if (!areQueriesFinished(poolSize, strat, run, fold, null, null, completeIt = false, expectedAmount + nclasses - 1)) error(s"Queries must be finished to check hits! |U|=$poolSize")
      else
         poolId(strat, learner, run, fold) match {
            case None => false
            case Some(pid) =>
               lazy val ExpectedHitsForNormalPool = expectedAmount
               val hs = (read(s"SELECT COUNT(1),max(t+0) FROM h WHERE p=$pid") match {
                  case List(Vector(0)) | List(Vector(0, 0)) => 0d
                  case List(Vector(c, m)) => if (c == m - nclasses + 2) c
                  else error(s"Inconsistency: $c cms differs from last timeStep+1 = ${m - nclasses + 2}")
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
                  val tuples = rest.zipWithIndex.map { case (patt, idx) =>
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

   /**
   q = primeiro t em que atinje max num dado pool
      mG = [(medianaG dos 25 qs p/ NB) + (medianaG dos 25 qs p/ C4.5) + (medianaG dos 25 qs p/ 5NN)] / 3
      mA = [(medianaA dos 25 qs p/ NB) + (medianaA dos 25 qs p/ C4.5) + (medianaA dos 25 qs p/ 5NN)] / 3
      m = maior(mG, mA) >= 50
      Qcalculado = m <= 100
      Q = Qcalculado >= |Y| + 2
     */
   //  def calculaQ(runs: Int, folds: Int, n: Int = n) {
   //    if (isQCalculated) quit("Q already calculated!")
   //    else {
   //      lazy val medianaTGme_s = 1 to 3 map { l =>
   //        val grpByPool = readBlobs4(s"select mat,t,r,f from h,p where h.p=p.id and s=0 and l=$l").map { case (b, t, r, f) =>
   //          (r, f) ->(gmeans(blobToConfusion(b, nclasses)), t)
   //        }.groupBy(_._1).map(_._2.map(_._2))
   //        val t_max_s = grpByPool.map { pool =>
   //          //max gmeans no pool
   //          val max = pool.maxBy(_._1)._1
   //          val maxs = pool.filter(x => x._1 >= max * 0.99)
   //          //primeiro t em que atinje max
   //          val t = maxs.minBy(_._2)._2
   //          (t, max)
   //        }.toSeq
   //        //pega mediana dentre 25 pools
   //        t_max_s.sortBy(_._1).get(t_max_s.size / 2 + 1)
   //      }
   //
   //      lazy val medianaTAcc_s = 1 to 3 map { l =>
   //        val grpByPool = readBlobs4(s"select mat,t,r,f from h,p where h.p=p.id and s=0 and l=$l").map { case (b, t, r, f) =>
   //          (r, f) ->(acc(blobToConfusion(b, nclasses)), t)
   //        }.groupBy(_._1).map(_._2.map(_._2))
   //        val t_max_s = grpByPool.map { pool =>
   //          val max = pool.maxBy(_._1)._1
   //          val maxs = pool.filter(x => x._1 >= max * 0.99)
   //          val t = maxs.minBy(_._2)._2
   //          (t, max)
   //        }.toSeq
   //        t_max_s.sortBy(_._1).get(t_max_s.size / 2 + 1)
   //      }
   //
   //      val Qcalculado = 50
   //      //      val Qcalculado = math.min(100, math.max(50, math.max(medianaTGme_s.map(_._1).sum / 3d, medianaTAcc_s.map(_._1).sum / 3d)))
   //
   //      val qToWrite = math.max(Qcalculado, nclasses + 2)
   //      write(s"INSERT INTO r values (0, -1, $qToWrite)")
   //    }
   //  }

   def countQueries(strat: Strategy, run: Int, fold: Int) = {
      val pid = poolId(strat, strat.learner, run, fold).getOrElse(quit(s"Pool not found for  s=${strat.id} and l=${strat.learner.id} and r=$run and f=$fold !"))
      read(s"SELECT COUNT(1) FROM q WHERE p=$pid") match {
         case List() => 0
         case List(seq) => seq.head.toInt
      }
   }

   /**
    * Get the list of all CMs sorted by time.
    * @param strat
    * @param learner
    * @param run
    * @param fold
    * @return
    */
   def getCMs(strat: Strategy, learner: Learner, run: Int, fold: Int, expectedAmount: Int) = poolId(strat, learner, run, fold) match {
      case None => error("Attempt to get hits without an existent related pid.")
      case Some(pid) =>
         val cms = mutable.LinkedHashMap[Int, Array[Array[Int]]]()
         //         readBlobs(s"select mat,t from h WHERE p=$pid ORDER BY t LIMIT $expectedAmount") foreach {
         readBlobs(s"select mat,t from h WHERE p=$pid ORDER BY t") foreach {
            case (b, t) => cms += t -> blobToConfusion(b, nclasses)
         }
         if (expectedAmount > cms.size) error(s"${cms.size} conf mats found, $expectedAmount expected!")
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
   def queries(strat: Strategy, run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
      val patts = read(s"SELECT i.id FROM i, q, p where i.id=q.i and p.id=p and p.s=${strat.id} and p.l=${strat.learner.id} and p.r=$run and p.f=$fold order by t asc") match {
         case List() => quit("No queries found!")
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

   def writeQueries(strat: Strategy, run: Int, fold: Int, q: Int) = {
      poolId(strat, strat.learner, run, fold) match {
         case Some(queryPoolId) => quit(s"Pool $run.$fold já estava gravado para $strat.${strat.learner} referente às queries a gravar.")
         case None =>
            val qs = strat.queries.take(q).toList
            val poolSQL = s"INSERT INTO p VALUES (NULL, ${strat.id}, ${strat.learner.id}, $run, $fold)"
            val sqls = poolSQL +: (qs.zipWithIndex map { case (patt, t) =>
               s"INSERT INTO q select id, $t, ${patt.id} from p where s=${strat.id} and l=${strat.learner.id} and r=$run and f=$fold"
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
   def writeHits(poolSize: Int, testSet: Seq[Pattern], queries: Vector[Pattern], strat: Strategy, run: Int, fold: Int, h: Int)(learner: Learner) =
      if (learner.id != strat.learner.id && strat.id > 1 && !Seq(211, 601, 361, 66361, 901, 391, 66391).contains(strat.id))
         error(s"Provided learner $learner is different from gnostic strategy's learner $strat.${strat.learner}")
      else {
         //Apenas agnostic strats gravam um poolId que tem NoLearner, não-reutilizável pra hits.
         val insertIntoP = poolId(strat, learner, run, fold) match {
            case Some(pid) => if (strat.id < 2 || Seq(211, 601, 361, 66361, 901, 391, 66391).contains(strat.id)) quit(s"Pool $run.$fold já estava gravado para $strat.$learner referente aos hits de $strat.") else "SELECT 1"
            case None =>
               if (strat.id < 2 || Seq(211, 601, 361, 66361, 901, 391, 66391).contains(strat.id)) s"INSERT INTO p VALUES (NULL, ${strat.id}, ${learner.id}, $run, $fold)"
               else quit(s"Missing gnostic queries pid for hits.")
         }

         //para rnd e quaisquer learners, |queries| = |U|.
         val expectedQ = if (strat.id == 0) poolSize else h + nclasses - 1
         if (expectedQ > queries.size) quit(s"Number of ${queries.size} provided queries for hits is lesser than $expectedQ expected!")

         //para rnd com learners especiais Q is not yet defined, pega |U|; senão pega apenas Q das queries fornecidas
         val qtdQueriesToTake = if (strat.id == 0 && learner.id < 4) poolSize else h + nclasses - 1
         val (initialPatterns, rest) = queries.take(qtdQueriesToTake).splitAt(nclasses)
         if (nclasses != initialPatterns.size || initialPatterns.size + rest.size != qtdQueriesToTake) error("Problems picking initialPatterns for hits.")

         //gera hits e sql strs
         var m = learner.build(initialPatterns)
         val tuples = (insertIntoP, null) +: ((null +: rest).zipWithIndex map { case (patt, idx) =>
            val t = idx + nclasses - 1
            if (patt != null) m = learner.update(m, fast_mutable = true)(patt)
            val cm = m.confusion(testSet)
            val blob = confusionToBlob(cm)
            (s"INSERT INTO h SELECT id, $t, ? FROM p where s=${strat.id} and l=${learner.id} and r=$run and f=$fold", blob)
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

   def getMeasure(measure: Measure, strategy: Strategy, learner: Learner, run: Int, fold: Int) = {
      val pid = poolId(strategy, learner, run, fold).getOrElse(quit(s"Pool ${(strategy, learner, run, fold)} not found!"))
      read(s"select v from r where p=$pid and m=${measure.id(this)}") match {
         case List() => None
         case List(seq) => Some(seq.head)
      }
   }

   def pids(sid: Int, lid: Int) = read(s"SELECT id FROM p WHERE s=$sid and l=$lid") match {
      case List() => None
      case l => Some(l.map(_.head.toInt))
   }

   def isMeasureComplete(measure: Measure, sid: Int, lid: Int) = {
      val Nrpools = Global.runs * Global.folds
      pids(sid, lid) match {
         case Some(l) if l.size == Nrpools => read(s"select count(v) from r where p in (${l.mkString(",")}) and m=${measure.id(this)}") match {
            case List(Vector(Nrpools)) => true
            case _ => false
         }
         case _ => false
      }
   }

   def measureToSQL(measure: Measure, value: Double, sid: Int, learner: Learner, run: Int, fold: Int) = {
      val pid = poolId(sid, learner.id, run, fold).getOrElse(quit(s"Pool ${(abr(sid), learner, run, fold)} not found!"))
      s"insert into r values (${measure.id(this)}, $pid, $value)"
   }

   def putMeasureValue(measure: Measure, value: Double, strategy: Strategy, learner: Learner, run: Int, fold: Int) {
      write(measureToSQL(measure, value, strategy.id, learner, run, fold))
   }


   //todo: completar abreviacoes pra novas estrats (apenas para pretty print e evitar unmatched key)
   lazy val abr = Seq(RandomSampling(Seq()),
      ClusterBased(Seq()),
      Uncertainty(NoLearner(), Seq()),
      Entropy(NoLearner(), Seq()),
      Margin(NoLearner(), Seq()),
      DensityWeighted(NoLearner(), Seq(), 1, "eucl"),
      DensityWeightedTrainingUtility(NoLearner(), Seq(), "cheb"),
      DensityWeightedTrainingUtility(NoLearner(), Seq(), "eucl"),
      DensityWeightedTrainingUtility(NoLearner(), Seq(), "maha"),
      DensityWeightedTrainingUtility(NoLearner(), Seq(), "manh"),
      MahalaWeightedTrainingUtility(NoLearner(), Seq(), 1, 1),
      ExpErrorReductionMargin(NoLearner(), Seq(), "entropy"),
      ExpErrorReductionMargin(NoLearner(), Seq(), "gmeans+residual"),
      ExpErrorReductionMargin(NoLearner(), Seq(), "accuracy"),
      new SGmulti(NoLearner(), Seq(), "consensus"),
      new SGmulti(NoLearner(), Seq(), "majority"),
      new SGmultiJS(NoLearner(), Seq()),
      SVMmulti(Seq(), "SELF_CONF"),
      SVMmulti(Seq(), "KFF"),
      SVMmulti(Seq(), "BALANCED_EE"),
      SVMmulti(Seq(), "BALANCED_EEw"),
      SVMmulti(Seq(), "SIMPLE")
   ).map(s => s.id -> s.abr).toMap
}