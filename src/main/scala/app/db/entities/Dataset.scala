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

package app.db.entities

import java.util.Calendar

import al.strategies.{RandomSampling, Strategy}
import ml.Pattern
import ml.classifiers._
import util.{Lazy, ALDatasets, Datasets, Tempo}
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable
import scala.util.Random

/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db que é um dataset.
 */
case class Dataset(path: String, createOnAbsence: Boolean = false, readOnly: Boolean = false)(dataset: String) extends Database {
  //for external use
  var finished: Boolean = false

  lazy val costAtAccMax = {

  }
  lazy val QFromRes = exec(s"select v from res where m=${fetchmid("Q")} and s=-1 and l=-1 and r=-1 and f=-1").get
  lazy val Qready = if (QFromRes.size > 1) unsafeQuit(s"More than one Q at $database .") else QFromRes.size == 1

  /**
   * Remove all hits with position greater than the position where the better learner (NB,5NN,C45)
   * in its better pool reaches the maximum accuracy.
   */
  def compactify() {
    //depois que compacta deixa-se de passar nos testes de HitsComplete!!!!!
    //Porém basta verificar se o Q já está definido.
    if (Qready) {
      //Faz lista com 25 Qs (um para cada pool); é o primeiro ponto de acc max do melhor dentre os 3 classificadores.
      val QNB_Q5NN_QC45 = (for {
        r <- (0 until runs).par
        f <- (0 until folds).par
        sql = s"select position from hit where run=$r and fold=$f and strategyid=1 and learnerid in (16,17,5) and pred=expe group by position,learnerid order by sum(value) desc, position asc limit 1"
      } yield {
        exec(sql).get.head.head
      }).toList

      //Pega max.
      val PosAccMax = QNB_Q5NN_QC45.sorted.toList.last.toInt
      println(s"posições de maximos: $QNB_Q5NN_QC45 posição do max: $PosAccMax")

      incCounter()
      acquireOp()
      exec("begin")
      exec(s"delete from hit where position>$PosAccMax")
      exec("end")
      //      exec(s"vacuum")
      //            save()
      releaseOp()

    }
  }

  lazy val Q = {
    if (!readOnly) {
      incCounter()
      acquireOp()
      exec("create table if not exists res ( m INT, s INT, l INT, r INT, f INT, v FLOAT, unique (m,s,l,r,f) on conflict rollback )")
      //save() Only save when really inserting ( = pseudo-transaction)
      releaseOp()
    }
    val alreadyCalculated = QFromRes.size > 0
    if (alreadyCalculated) QFromRes.head.head.toInt
    else {
      if (!rndHitsComplete(NB()) || !rndHitsComplete(KNNBatch(5, "eucl", Seq(), weighted = true)) || !rndHitsComplete(C45())) -1 //-1: just warming
      else {
        //Faz lista com 25 Qs (um para cada pool); é o primeiro ponto de acc max do melhor dentre os 3 classificadores.
        val QNB_Q5NN_QC45 = (for {
          r <- (0 until runs).par
          f <- (0 until folds).par
          sql = s"select position from hit where run=$r and fold=$f and strategyid=1 and learnerid in (16,17,5) and pred=expe group by position,learnerid order by sum(value) desc, position asc limit 1"
        } yield {
          exec(sql).get.head.head
        }).toList

        //Pega mediana.
        val QAccMax = QNB_Q5NN_QC45.sorted.toList(runs * folds / 2).toInt
        println(s"Q=$QAccMax")

        incCounter()
        acquireOp()
        exec(s"insert into res values (1,-1,-1,-1,-1,$QAccMax)") //todo: aqui quebra caso db esteja aberto como readOnly
        save()
        releaseOp()

        QAccMax
      }
    }
  }

  /**
   * checa se tabela de matrizes de confusão está completa para todos os pools inteiros para Random/learner
   * Se Q está pronto, então queries tão completas (para acelerar e também permitir que bases sejam compactadas apagado-se hits em excesso)
   */
  def rndHitsComplete(learner: Learner) = if (Qready) true
  else {
    val expectedHits = n * (folds - 1) * runs
    val hitExs = countPerformedConfMatrices(RandomSampling(Seq()), learner)
    if (hitExs > expectedHits) justQuit(s"$hitExs confusion matrices of Rnd cannot be greater than $expectedHits for $this with $learner")
    else hitExs == expectedHits || hitExs >= 10000
  }

  lazy val nclasses = exec(s"select max(Class+1) from inst").get.head.head.toInt
  lazy val n = exec(s"select count(*) from inst").get.head.head.toInt
  lazy val countRndStartedPools = exec(s"select * from query where strategyid=1 group by run,fold").get.length
  lazy val countRndPerformedQueries = exec(s"select count(*) from query ${where(RandomSampling(Seq()), NoLearner())}").get.head.head.toInt
  val database = dataset
  val sidmap = mutable.Map[String, Int]()
  val lidmap = mutable.Map[String, Int]()
  val midmap = mutable.Map[String, Int]()

  /**
   * Returns only the recorded number of tuples.
   * You should add runs*folds*|Y|²*|Y| manually.
   */
  //  def rndCompleteHits(learner: Learner) = exec(s"select count(*) from hit ${where(RandomSampling(Seq()), learner)}").get.head.head.toInt

  def where(strategy: Strategy, learner: Learner) = s" where strategyid=${fetchsid(strategy)} and learnerid=${fetchlid(learner)}"

  def fetchmid(str: String) = {
    val sql = "select rowid from medida where name='" + str + "'"
    //Fetch MedidaId by name.
    lazy val mid = try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(sql)
      resultSet.next()
      val r = resultSet.getInt("rowid")
      resultSet.close()
      r
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems consulting medida from " + dbCopy + s" with query '$sql'.")
    }
    midmap.getOrElseUpdate(str, mid)
  }

  def fetchlid(learner: Learner) = {
    val sql = "select rowid from app.learner where name='" + learner + "'"
    //Fetch LearnerId by name.
    lazy val lid = try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(sql)
      resultSet.next()
      val r = resultSet.getInt("rowid")
      resultSet.close()
      r
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems consulting learner from " + dbCopy + s" with query '$sql'.")
    }
    lidmap.getOrElseUpdate(learner.toString, lid)
  }

  def fetchsid(strat: Strategy) = {
    //Fetch StrategyId by name.
    lazy val sid = try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("select rowid from app.strategy where name='" + strat + "'")
      resultSet.next()
      val r = resultSet.getInt("rowid")
      resultSet.close()
      r
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems consulting strategy from " + dbCopy + " with query \"" + "select rowid from app.strategy where name='" + strat + "'" + "\".")
    }
    sidmap.getOrElseUpdate(strat.toString, sid)
  }

  def saveHits(strat: Strategy, learner: Learner, run: Int, fold: Int, nc: Int, f: Standardize, testSet: Seq[Pattern], seconds: Double, pattsFromARFFMap: Map[Int, Pattern], Q: Int = Int.MaxValue) {
    if (exiting()) return //se estava saindo, nem começa novo lote
    if (readOnly) justQuit("Cannot save queries on a readOnly database!")

    if (!isOpen) justQuit(s"Impossible to get connection to write queries at the run $run and fold $fold for strategy $strat and learner ${strat.learner}. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")

    if (!fileLocked) justQuit(s"This thread has is not in charge of locking $database . Impossible to get connection to write queries at the run $run and fold $fold for strategy $strat and learner ${strat.learner}.")

    val lid = fetchlid(learner)
    val sid = fetchsid(strat)

    //descobre em que ponto das queries retomar os hits
    val nextPos = nextHitPosition(strat, learner, run, fold)
    val timeStep = math.max(nc, nextPos)
    //    if (timeStep==3) println(s"next=3: $strat $learner $run $fold")
    val queries = fetchQueries(strat, run, fold, f, pattsFromARFFMap) ////////////////

    //retoma hits
    val initial = queries.take(timeStep)
    val rest = queries.drop(timeStep)
    if (timeStep > Q) println(s"Warning: there are more confusion mat. ($timeStep) than desired ($Q).")
    if (rest.nonEmpty && timeStep < Q) {
      var model = learner.build(initial)

      //train
      val ti = System.currentTimeMillis()
      val results = mutable.Queue[String]()
      println(s"A processar ${rest.toList.size}; mas apenas para totalizar $Q e desde tempo $timeStep, ${(Q - timeStep) * nclasses} hits (exiting:${exiting()})...")
      incCounter()
      rest.zipWithIndex.take(Q - timeStep).toStream.takeWhile(_ => (System.currentTimeMillis() - ti) / 1000.0 < seconds && !exiting()).foreach {
        case (trainingPattern, idx) =>
          model = learner.update(model, fast_mutable = true)(trainingPattern)
          val confusion = model.confusion(testSet)
          val position = timeStep + idx
          var i = 0
          var j = 0
          while (i < nc) {
            j = 0
            while (j < nc) {
              val sql = s"insert into hit values ($sid, $lid, $run, $fold, $position, $i, $j, ${confusion(i)(j)})"
              results.enqueue(sql)
              j += 1
            }
            i += 1
          }
      }

      //save
      batchWrite(results.toArray)
      println(s"${results.size} hits inserted into $database for $strat / $learner!")
    }
  }

  def nextHitPosition(strategy: Strategy, learner: Learner, run: Int, fold: Int) = countEvenWhenEmpty(" from hit " + where(strategy, learner) + s" and run=$run and fold=$fold").head.head.toInt

  /**
   * Only for specific pool and fold (with where clause or sumarizing with group by).
   * @param s
   * @param offset
   * @return
   */
  def countEvenWhenEmpty(s: String, offset: Int = 0) = {
    //    println(s"select (max(position)+1+$offset) " + s)
    val n = exec("select count(*) " + s).get.map(_.head.toInt).sum
    if (n == 0) mutable.Queue(Seq.fill(5)(0d))
    else exec(s"select (max(position)+1+$offset) " + s).get
  }

  /**
   * Including first |Y| imaginary matrices.
   * Also checks consistency position-count for the pool.
   */
  def countPerformedConfMatricesForPool(strategy: Strategy, learner: Learner, run: Int, fold: Int) = {
    val q = countPerformedQueriesForPool(strategy, run, fold)
    val c0 = exec(s"select count(*) from hit ${where(strategy, learner)} and run=$run and fold=$fold").get.head.head.toInt / (nclasses * nclasses).toDouble
    val c = if (c0 == 0) 0 else c0 + nclasses
    val m = countEvenWhenEmpty(s" from hit ${where(strategy, learner)} and run=$run and fold=$fold").head.head.toInt
    if (c > q) safeQuit(s"Inconsistency at $strategy / $learner: number of queries \n$q\n at run $run and fold $fold for $dataset is lesser than number of conf. matrices \n$c\n .")
    if (c != m) safeQuit(s"Inconsistency at $strategy / $learner: max position +1 $m at run $run and fold $fold for $dataset differs from number of conf. matrices $c .")
    c
  }

  //  def countPerformedQueriesForPool(strategy: Strategy, run: Int, fold: Int) = countEvenWhenEmpty(s"from query,${where(strategy, strategy.learner)} and run=$run and fold=$fold").head.head.toInt
  //
  //  def countPerformedQueries(strategy: Strategy) = exec(s"select count(*) from query,${where(strategy, strategy.learner)}").head.head.toInt

  /**
   * Including first |Y| imaginary matrices.
   * Also checks consistency position-count of each pool.
   */
  def countPerformedConfMatrices(strategy: Strategy, learner: Learner) = {
    val q = countPerformedQueriesList(strategy)
    val mx_cn = countEvenWhenEmpty(s", count(*)/(${nclasses * nclasses}*1.0)+$nclasses from hit ${where(strategy, learner)} group by run,fold")
    val m = mx_cn.map(_.head)
    val c = mx_cn.map(_.tail.head)
    if (c.size > q.size) safeQuit(s"countPerformedConfMatrices: Inconsistency at $strategy / $learner: number of query pools \n${q.size}\n\n for $dataset is lesser than number of conf. matrices pools \n${c.size}\n .")
    if (c.size == q.size && c.zip(q).exists { case (a, b) => a > b}) safeQuit(s"countPerformedConfMatrices: Inconsistency at $strategy / $learner: number of queries \n$q\n\n for $dataset is lesser than total number of conf. matrices \n$c\n in some pool.")
    if (c != m) safeQuit(s"countPerformedConfMatrices: Inconsistency at $strategy / $learner: max positions +1 \n$m\n for $dataset differs from total number of conf. matrices \n$c\n .")
    c.sum.toInt
  }

  /**
   * Number of started pools for the given strat and its learner.
   * Not necessarily complete ones.
   * @param strategy
   * @return
   */
  def startedPools(strategy: Strategy) =
    exec(s"select * from query ${where(strategy, strategy.learner)} group by run,fold").get.length

  /**
   * Also checks consistency position-count for the pool.
   * @param strategy
   * @param run
   * @param fold
   * @return
   */
  def countPerformedQueriesForPool(strategy: Strategy, run: Int, fold: Int) = {
    val learner = strategy.learner
    val c = exec(s"select count(*) from query ${where(strategy, learner)} and run=$run and fold=$fold").get.head.head.toInt
    val m = countEvenWhenEmpty(s" from query ${where(strategy, learner)} and run=$run and fold=$fold").head.head.toInt
    if (c != m) safeQuit(s"Inconsistency at $strategy / $learner: max position +1 $m at run $run and fold $fold for $dataset differs from number of queries $c .")
    c
  }

  /**
   * Also checks consistency position-count of each pool.
   * @param strategy
   * @return
   */
  def countPerformedQueries(strategy: Strategy) = countPerformedQueriesList(strategy).sum.toInt


  def countPerformedQueriesList(strategy: Strategy) = {
    val learner = strategy.learner
    val mx_cn = countEvenWhenEmpty(s", count(*) from query ${where(strategy, learner)} group by run,fold")
    val m = mx_cn.map(_.head)
    val c = mx_cn.map(_.tail.head)

    if (c != m) safeQuit(s"Inconsistency at $strategy / $learner: seq of max positions +1 \n$m\n for $dataset differs from seq of number of queries \n$c\n .")
    c
  }

  /**
   * Inserts query-tuples (run, fold, position, instid) into database.
   * All queries for a given pair run-fold should be written at once or with the limits below.
   * The original file is updated at the end.
   * If the given pair run/fold already exists, queries are resumed from the last one recorded.
   *
   * Generates queries until Q from the provided strategy,
   * unless Q is not given (in this case all possible queries (the entire pool) will be generated)
   * or a time restriction is specified.
   *
   * filter f is needed to resume queries
   *
   * @param strat
   * @param run
   * @param fold
   * @param seconds time limit, if exceeded, waits for the current query to end and finish querying
   * @param Q
   * @return the total number of queries generated by the strategy along all jobs in this pool
   */
  def saveQueries(strat: Strategy, run: Int, fold: Int, f: Standardize, seconds: Double, pattsFromARFFMap: Map[Int, Pattern], Q: Int = Int.MaxValue, allWin: Boolean = false) =
    if (exiting()) None //se estava saindo, nem começa novo lote
    else {
      if (!isOpen) justQuit(s"Impossible to get connection to write queries at the run $run and fold $fold for strategy $strat and learner ${strat.learner}. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      if (readOnly) unsafeQuit("Cannot save queries on a readOnly database!")
      if (!fileLocked) justQuit(s"This thread has is not in charge of locking $database . Impossible to get connection to write queries at the run $run and fold $fold for strategy $strat and learner ${strat.learner}.")
      val stratId = fetchsid(strat)
      val learnerId = fetchlid(strat.learner)

      //Check if there are more queries than the size of the pool (or greater than Q).
      var q = -1
      try {
        val statement = connection.createStatement()
        val resultSet = statement.executeQuery(s"select count(rowid) as q from query where strategyid=$stratId and learnerid=$learnerId and run=$run and fold=$fold")
        resultSet.next()
        q = resultSet.getInt("q")
        resultSet.close()
        if (q > strat.pool.size) justQuit("Excess of queries (" + q + ") fetched from dataset " + dbCopy + " for run=" + run + " and fold=" + fold + s" for $strat / ${strat.learner}. They are greater than Q " + Q + s" or pool size ${strat.pool.size}. ")
      } catch {
        case e: Throwable => e.printStackTrace
          safeQuit("\nProblems looking for preexistence of queries in: " + dbCopy + ".")
      }

      //Get queries from past jobs, if any.
      val queries = fetchQueries(strat, run, fold, f, pattsFromARFFMap) ////////////////
      val nextPosition = queries.size
      val r = if (nextPosition < Q && nextPosition < strat.pool.size) {
        println(s"${Calendar.getInstance().getTime} Gerando $strat queries na posição $nextPosition de um total de ${if (Q == Int.MaxValue) strat.pool.size else Q} queries para $dataset pool: $run.$fold ...")
        incCounter()
        val (nextIds, t) = if (nextPosition == 0) Tempo.timev(strat.timeLimitedQueries(seconds, exiting).take(Q).map(_.id).toVector)
        else Tempo.timev(strat.timeLimitedResumeQueries(queries, seconds, exiting).take(Q - nextPosition).map(_.id).toVector)
        q = nextIds.length

        if (nextPosition == 0 && q < nclasses) {
          println(s"Interrupted querying did not get enough queries to write ($q < $nclasses).")
          0
        } else {

          if (allWin && nextPosition + q != Q) println(s"In allWin mode: ($nextPosition + $q) generated queries != $Q expected queries; ignoring write request.")
          else {
            acquireOp()
            println(s"Gravando queries para $dataset pool: $run.$fold ...")
            var str = ""
            try {
              val statement = connection.createStatement()
              statement.executeUpdate("begin")
              nextIds.zipWithIndex.foreach { case (pattId, idx) =>
                val position = nextPosition + idx
                str = s"insert into query values ($stratId,$learnerId,$run,$fold,$position,$pattId)"
                statement.executeUpdate(str)
              }
              statement.executeUpdate("end")
              statement.close()
              println("Queries written.")
            } catch {
              case e: NullPointerException =>
                releaseOp()
                println(e.getMessage)
                justQuit(s"\nProblems inserting queries for $strat / ${strat.learner} into: $dbCopy: [ $str ].")
              case e: Throwable => e.printStackTrace
                releaseOp()
                println(s"\nProblems inserting queries for $strat / ${strat.learner} into: $dbCopy: [ $str ]:")
                println(e.getMessage)
                safeQuit(s"\nProblems inserting queries for $strat / ${strat.learner} into: $dbCopy: [ $str ].")
            }
            println(s"${Calendar.getInstance().getTime} $q $strat queries written to " + dbCopy + s" at $run.$fold.")
            weakSave()
            //          save()
            releaseOp()
          }
          nextPosition + q
        }
      } else nextPosition
      Some(r)
    }

  /**
   * If f==nul gets original attributes from ARFF,
   * else gets numericized attributes from SQLite applying zscore to them.
   * @param strat
   * @param run
   * @param fold
   * @param f
   * @return
   */
  def fetchQueries(strat: Strategy, run: Int, fold: Int, f: Standardize, pattsFromARFFMap: Map[Int, Pattern]) = {
    //    incCounter() //tirei lock daqui, pois busy_timeout está bem grande agora
    //    acquireOp()
    val qs = if (f == null) ALDatasets.queriesFromMap(path + dataset + ".arff")(this)(strat, run, fold, pattsFromARFFMap)
    else ALDatasets.queriesFromSQLite(this)(strat, run, fold)

    val queries = qs match {
      case Right(x) => x
      case Left(str) =>
        safeQuit(s"Problem loading queries for Rnd: $str.")
    }
    //    releaseOp()
    if (f != null) Datasets.applyFilter(f)(queries) else queries //não é necessário reordenar, pois essa aplicação de filtro deve preservar a ordem original das queries
  }
}


//object DatasetTest extends App {
//  val l = "appendicitis,blogger,glioma16,fertility-diagnosis,planning-relax,qualitative-bankruptcy,lenses,acute-inflammations-urinary,lung-cancer,post-operative-patient,dbworld-subjects-stemmed,iris,robot-failure-lp3,zoo,leukemia-haslinger,dbworld-bodies-stemmed,volcanoes-d1,hepatitis,movement-libras-1,robot-failure-lp2,heart-disease-processed-switzerland,habermans-survival,robot-failure-lp4,robot-failure-lp1,hayes-roth,volcanoes-d3,teaching-assistant-evaluation,wine,lsvt-voice-rehabilitation,breast-tissue-6class,seeds,led7digit,heart-disease-processed-hungarian,ozone-eighthr,volcanoes-d4,molecular-promotor-gene,voting,breast-tissue-4class,statlog-heart,thyroid-newthyroid,monks3,breast-cancer-wisconsin,spectf-heart,volcanoes-d2,heart-disease-processed-cleveland,heart-disease-processed-va,steel-plates-faults,meta-data,lymphography,monks1,cardiotocography-10class,flare,robot-failure-lp5,spect-heart,flags,parkinsons,vertebra-column-2c,vertebra-column-3c,arcene,systhetic-control,ionosphere,dresses-sales,horse-colic-surgical,connectionist-mines-vs-rocks,glass,bupa,heart-disease-reprocessed-hungarian,dermatology,indian-liver-patient,mammographic-mass,ecoli,blood-transfusion-service,wholesale-channel,movement-libras-10,ozone-onehr,climate-simulation-craches,wdbc,user-knowledge,arrhythmia,volcanoes-e2,micro-mass-mixed-spectra,saheart,credit-approval,movement-libras,statlog-australian-credit,waveform-v1,pima-indians-diabetes,leaf,volcanoes-e4,volcanoes-e1,balance-scale,autoUniv-au6-cd1-400,volcanoes-a1,banknote-authentication,monks2,autoUniv-au7-cpd1-500,volcanoes-e5,connectionist-vowel-reduced,wine-quality-red,autoUniv-au7-700,volcanoes-a4,waveform-v2,micro-mass-pure-spectra,autoUniv-au6-250-drift-au6-cd1-500,annealing,statlog-german-credit-numeric,autoUniv-au1-1000,tic-tac-toe,statlog-vehicle-silhouettes,autoUniv-au6-1000,volcanoes-a2,ringnorm,statlog-german-credit,yeast-4class,qsar-biodegradation,volcanoes-e3,volcanoes-a3,autoUniv-au7-300-drift-au7-cpd1-800,connectionist-vowel,cnae-9,yeast,thyroid-hypothyroid,cmc,hill-valley-with-noise,mushroom-expanded,mushroom,hill-valley-without-noise,digits2,thyroid-sick-euthyroid,twonorm,cardiotocography-3class,robot-nav-sensor-readings-2,semeion,multiple-features,car-evaluation,statlog-image-segmentation,mfeat-fourier,robot-nav-sensor-readings-4,thyroid-allrep,thyroid-dis,thyroid-allhyper,thyroid-allhypo,thyroid-allbp,kr-vs-kp,thyroid-ann,wine-quality-white-5class,thyroid-sick,abalone-11class,volcanoes-b3,statlog-landsat-satellite,turkiye-student,molecular-splice-junction,wilt,abalone-3class,volcanoes-b5,volcanoes-b4,spambase,volcanoes-b1,wine-quality-5class,page-blocks,artificial-characters,banana,volcanoes-b6,robot-nav-sensor-readings-24,optdigits,texture,phoneme,first-order-theorem,volcanoes-b2,musk,pendigits,gas-drift-different-concentrations,eeg-eye-state,gas-drift,nursery,nursery-4class,magic".split(",")
//  val res = l forall { d =>
//    val da = Dataset("/home/davi/wcs/ucipp/uci/new/", false, true)(d)
//    val db = Dataset("/home/davi/wcs/ucipp/uci/new/", false, true)(d)
//    da.open()
//    db.open()
//
//    val patts = Datasets.arff(true)("/home/davi/wcs/ucipp/uci/" + da.database + ".arff", false, false).right.get
//    val m = patts.map(p => p.id -> p).toMap
//
//    val a = ALDatasets.queriesFromMap(da.path + da.database + ".arff")(da)(RandomSampling(Seq()), 0, 0, m).right.get
//    val b = ALDatasets.queriesFromSQLite(db)(RandomSampling(Seq()), 0, 0).right.get
//    //    val a = Datasets.patternsFromSQLite(da.path)(da.database).right.get
//    //    val b = Datasets.patternsFromSQLite(db.path)(db.database).right.get
//    //    val b = Datasets.arff(true)(db.path + db.database + ".arff", false, false).right.get
//    val sa = a.sameElements(b)
//    a.toList take 7 foreach (x => println(x.toStringCerto.count(_ == ',')))
//    println("-----")
//    b.toList take 7 foreach (x => println(x.toStringCerto.count(_ == ',')))
//    println(sa)
//    println("")
//    da.close()
//    db.close()
//    sa
//  }
//  println("tudo igual : " + res)
//  sys.exit(1)
//  //load patterns
//  //  val patts = Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("iris").right.get
//
//  //reorder patterns as queries
//  //  val shuffled = patts.drop(5) ++ patts.take(4)
//
//  //  val d = Dataset("/home/davi/wcs/ucipp/uci/")("arcene")
//  val d = Dataset("/home/davi/wcs/ucipp/uci/")("abalone-3class")
//  d.open()
//  //  d.saveQueries(RandomSampling(patts), 64, 17, 0.2)
//
//  //load queries as patterns
//  val qpattss = ALDatasets.queriesFromSQLite(Dataset("/home/davi/wcs/ucipp/uci/")("abalone-3class"))(RandomSampling(Seq()), 0, 0) match {
//    case Right(x) => x
//    case Left(str) => println(s"Problema: $str"); ???
//  }
//  qpattss take 10 foreach println
//
//  //  val qpattsa = ALDatasets.queriesFromARFF("/home/davi/wcs/ucipp/uci/iris.arff")(d)(RandomSampling(Seq()), 0, 0,) match {
//  //    case Right(x) => x
//  //    case Left(str) => println(s"Problema: $str"); ???
//  //  }
//
//
//  //  d.exec("select  rowid,* from inst").get foreach println
//  println(d.compactify())
//  d.close()
//
//}
