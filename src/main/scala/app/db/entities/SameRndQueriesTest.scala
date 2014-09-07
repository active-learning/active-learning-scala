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
 * Verifica se a sequencia aleatoria permanece a mesma
 * desde ARFFToSQLite até agora em que os atributos vêm direto do ARFF.
 */
object SameRndQueriesTest extends App {
  val l = "appendicitis,blogger,glioma16,fertility-diagnosis,planning-relax,qualitative-bankruptcy,lenses,acute-inflammations-urinary,lung-cancer,post-operative-patient,dbworld-subjects-stemmed,iris,robot-failure-lp3,zoo,leukemia-haslinger,dbworld-bodies-stemmed,volcanoes-d1,hepatitis,movement-libras-1,robot-failure-lp2,heart-disease-processed-switzerland,habermans-survival,robot-failure-lp4,robot-failure-lp1,hayes-roth,volcanoes-d3,teaching-assistant-evaluation,wine,lsvt-voice-rehabilitation,breast-tissue-6class,seeds,led7digit,heart-disease-processed-hungarian,ozone-eighthr,volcanoes-d4,molecular-promotor-gene,voting,breast-tissue-4class,statlog-heart,thyroid-newthyroid,monks3,breast-cancer-wisconsin,spectf-heart,volcanoes-d2,heart-disease-processed-cleveland,heart-disease-processed-va,steel-plates-faults,meta-data,lymphography,monks1,cardiotocography-10class,flare,robot-failure-lp5,spect-heart,flags,parkinsons,vertebra-column-2c,vertebra-column-3c,arcene,systhetic-control,ionosphere,dresses-sales,horse-colic-surgical,connectionist-mines-vs-rocks,glass,bupa,heart-disease-reprocessed-hungarian,dermatology,indian-liver-patient,mammographic-mass,ecoli,blood-transfusion-service,wholesale-channel,movement-libras-10,ozone-onehr,climate-simulation-craches,wdbc,user-knowledge,arrhythmia,volcanoes-e2,micro-mass-mixed-spectra,saheart,credit-approval,movement-libras,statlog-australian-credit,waveform-v1,pima-indians-diabetes,leaf,volcanoes-e4,volcanoes-e1,balance-scale,autoUniv-au6-cd1-400,volcanoes-a1,banknote-authentication,monks2,autoUniv-au7-cpd1-500,volcanoes-e5,connectionist-vowel-reduced,wine-quality-red,autoUniv-au7-700,volcanoes-a4,waveform-v2,micro-mass-pure-spectra,autoUniv-au6-250-drift-au6-cd1-500,annealing,statlog-german-credit-numeric,autoUniv-au1-1000,tic-tac-toe,statlog-vehicle-silhouettes,autoUniv-au6-1000,volcanoes-a2,ringnorm,statlog-german-credit,yeast-4class,qsar-biodegradation,volcanoes-e3,volcanoes-a3,autoUniv-au7-300-drift-au7-cpd1-800,connectionist-vowel,cnae-9,yeast,thyroid-hypothyroid,cmc,hill-valley-with-noise,mushroom-expanded,mushroom,hill-valley-without-noise,digits2,thyroid-sick-euthyroid,twonorm,cardiotocography-3class,robot-nav-sensor-readings-2,semeion,multiple-features,car-evaluation,statlog-image-segmentation,mfeat-fourier,robot-nav-sensor-readings-4,thyroid-allrep,thyroid-dis,thyroid-allhyper,thyroid-allhypo,thyroid-allbp,kr-vs-kp,thyroid-ann,wine-quality-white-5class,thyroid-sick,abalone-11class,volcanoes-b3,statlog-landsat-satellite,turkiye-student,molecular-splice-junction,wilt,abalone-3class,volcanoes-b5,volcanoes-b4,spambase,volcanoes-b1,wine-quality-5class,page-blocks,artificial-characters,banana,volcanoes-b6,robot-nav-sensor-readings-24,optdigits,texture,phoneme,first-order-theorem,volcanoes-b2,musk,pendigits,gas-drift-different-concentrations,eeg-eye-state,gas-drift,nursery,nursery-4class,magic".split(",")
  val asd = false
  val res = l forall { d =>
    val patts = Lazy(new Random(0).shuffle(Datasets.arff(false)("/home/davi/wcs/ucipp/uci/" + d + ".arff", false, false).right.get))
    val run = 0
    val (pool, f, fold) = Datasets.kfoldCV(patts, 5) {
      (tr0, ts0, fold, _) =>
        lazy val f = Datasets.zscoreFilter(tr0)

        lazy val pool = if (asd) {
          val tr = Datasets.applyFilterChangingOrder(tr0, f) //weka is unpredictable: without lazy resulting z-score values differ
          new Random(run * 100 + fold).shuffle(tr)
        } else {
          val tr = Datasets.applyFilter(tr0, f)
          new Random(run * 100 + fold).shuffle(tr0.zip(tr).sortBy(_._2.vector.toString()).map(_._1)) //changes order like would occurs inside filter and then shuffles
        }


        lazy val testSet = if (asd) {
          val ts = Datasets.applyFilterChangingOrder(ts0, f)
          new Random(run * 100 + fold).shuffle(ts)
        } else {
          val ts = Datasets.applyFilter(ts0, f)
          new Random(run * 100 + fold).shuffle(ts0.zip(ts).sortBy(_._2.vector.toString()).map(_._1)) //changes order like would occurs inside filter and then shuffles
        }

        (pool, f, fold)
    }.head
    println(fold)

    val da = Dataset("/home/davi/wcs/ucipp/uci/", false, readOnly = false)(d)
    val db = Dataset("/home/davi/wcs/ucipp/uci/new/", false, true)(d)
    da.open()
    db.open()
    println(da.n)
    println(db.n)

    //    val patts = Datasets.arff(true)("/home/davi/wcs/ucipp/uci/" + da.database + ".arff", false, false).right.get
    val m = if (asd) null else patts.map(p => p.id -> p).toMap

    //    val a = ALDatasets.queriesFromMap(da.path + da.database + ".arff")(da)(RandomSampling(Seq()), 0, 0, m).right.get
    //    val b = ALDatasets.queriesFromSQLite(db)(RandomSampling(Seq()), 0, 0).right.get
    //

    da.saveQueries(RandomSampling(pool), 0, 0, if (asd) f else null, 234234, m)
    da.save()
    val a = ALDatasets.queriesFromMap(da.path + da.database + ".arff")(da)(RandomSampling(Seq()), 0, 0, m).right.get
    //    val a = ALDatasets.queriesFromSQLite(da)(RandomSampling(Seq()), 0, 0).right.get
    val b = ALDatasets.queriesFromSQLite(db)(RandomSampling(Seq()), 0, 0).right.get

    //    val a = Datasets.patternsFromSQLite(da.path)(da.database).right.get
    //    val b = Datasets.patternsFromSQLite(db.path)(db.database).right.get
    //    val b = Datasets.arff(true)(db.path + db.database + ".arff", false, false).right.get
    val sa = a.sameElements(b)
    //    a.toList take 7 foreach (x => println(x.toStringCerto.count(_ == ',')))
    //    a.toList take 7 foreach (x => println(x.dataset()))
    a.toList take 7 foreach (x => println(x.id + " " + x.toStringCerto))
    println("-----")
    //    b.toList take 7 foreach (x => println(x.toStringCerto.count(_ == ',')))
    //    b.toList take 7 foreach (x => println(x.dataset()))
    b.toList take 7 foreach (x => println(x.id + " " + x.toStringCerto))
    println(sa)
    println("")
    da.close()
    db.close()
    sa
  }
  println("tudo igual : " + res)
}