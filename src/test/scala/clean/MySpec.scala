//package clean
//
//import ml.classifiers.NB
//import util.Datasets
//
//import scala.io.Source
//import scala.util.Random
//
///*
// active-learning-scala: Active Learning library for Scala
// Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//class MySpec extends UnitSpec with Blob with Lock {
//  lazy val datasets = Source.fromFile("juntos.txt").getLines().toList.par
//  val path = "/home/davi/testuci"
//
//  "Database" should "create a table, write and read two tuples" in {
//    val db = new Db(Global.appPath + "/test.db")
//    db.open()
//    assert(db.write("drop table if exists test") ===())
//    assert(db.write("create table test (a INT, b FLOAT)") ===())
//    assert(db.write("insert into test values (7, 0.7)") ===())
//    assert(db.write("insert into test values (8, 0.8)") ===())
//    assertResult(List(Vector(7, 0.7), Vector(8, 0.8)))(db.read("select * from test"))
//    db.close()
//  }
//
//  val d = Ds(Global.appPath, "flags-colour")
//  d.open()
//  val l = NB()
//  val m = l.build(d.patterns.take(20))
//  d.close()
//  "Dataset" should s"blob" in {
//    val db = new Db(Global.appPath + "/test.db")
//    db.open()
//    assert(db.write("drop table if exists h") ===())
//    assert(db.write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) )") ===())
//    val a = Array(3.toByte, 5.toByte, 255.toByte, 0.toByte, 24.toByte)
//    db.writeBlob("insert into h values (1, 3, ?)", a)
//    assert(a.sameElements(db.readBlobs("select mat,0 from h where p=1 and t=3").head._1))
//    db.close()
//  }
//
//  if (true) datasets.par foreach { d =>
//    val db = Ds(path, d)
//    db.open()
//    val l = NB()
//    val m = l.build(db.patterns.take(db.nclasses * 2))
//    val mat = m.confusion(db.patterns.drop(db.nclasses * 2).take(db.nclasses * 3))
//    db.write("drop table if exists h")
//    db.write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) )")
//    db.writeBlob("insert into h values (1, 32, ?)", confusionToBlob(mat))
//    val blob = db.readBlobs("select mat,0 from h where p=1 and t=32").head._1
//    val writtenMat = blobToConfusion(blob, db.nclasses)
//    db.close()
//    acquire()
//    s"Dataset $db with ${db.nclasses} classes" should s"shrink, write, read and stretch a confusion matrix" in  {
//      assertResult(mat)(writtenMat)
//    }
//    release()
//  }
//
//  "All dataset db files" should "have ids matching ARFF line numbers" in {
//    //label is not checked because there could be mismatching due to deduplication
//    val okOrProjectedOrRemovedAttsOrMissingValues = ("bank-marketing,appendicitis,blogger,glioma16,fertility-diagnosis,planning-relax," +
//      "qualitative-bankruptcy,lenses,acute-inflammations-urinary,lung-cancer,post-operative-patient,dbworld-subjects," +
//      "iris,robot-failure-lp3,zoo,leukemia-haslinger,dbworld-bodies,volcanoes-d1,hepatitis,movement-libras-1," +
//      "robot-failure-lp2,heart-disease-processed-switzerland,habermans-survival,robot-failure-lp4,robot-failure-lp1," +
//      "hayes-roth,volcanoes-d3,teaching-assistant-evaluation,wine,lsvt-voice-rehabilitation,breast-tissue-6class,seeds," +
//      "led7digit,heart-disease-processed-hungarian,ozone-eighthr,volcanoes-d4,molecular-promotor-gene,voting," +
//      "breast-tissue-4class,statlog-heart,thyroid-newthyroid,monks3,breast-cancer-wisconsin,spectf-heart,volcanoes-d2," +
//      "heart-disease-processed-cleveland,heart-disease-processed-va,steel-plates-faults,meta-data,lymphography,monks1," +
//      "cardiotocography-10class,flare,robot-failure-lp5,spect-heart,flags-religion,flags-colour,parkinsons," +
//      "vertebra-column-2c,vertebra-column-3c,arcene,systhetic-control,ionosphere,horse-colic-surgical," +
//      "connectionist-mines-vs-rocks,glass,bupa,heart-disease-reprocessed-hungarian,dermatology,indian-liver-patient," +
//      "mammographic-mass,ecoli,blood-transfusion-service,wholesale-channel,movement-libras-10,ozone-onehr," +
//      "climate-simulation-craches,wdbc,user-knowledge,arrhythmia,volcanoes-e2,micro-mass-mixed-spectra,saheart," +
//      "credit-approval,movement-libras,statlog-australian-credit,waveform-v1,pima-indians-diabetes,leaf,volcanoes-e4," +
//      "volcanoes-e1,balance-scale,autoUniv-au6-cd1-400,volcanoes-a1,banknote-authentication,monks2,autoUniv-au7-cpd1-500," +
//      "volcanoes-e5,connectionist-vowel-reduced,wine-quality-red,autoUniv-au7-700,volcanoes-a4,waveform-v2," +
//      "micro-mass-pure-spectra,autoUniv-au6-250-drift-au6-cd1-500,annealing").split(",")
//    datasets.filter(!okOrProjectedOrRemovedAttsOrMissingValues.contains(_)) foreach { dataset =>
//      val source = Source.fromFile(s"/home/davi/wcs/ucipp/uci/$dataset.arff")
//      val arff = source.getLines().dropWhile(!_.contains("@data")).toList.tail.zipWithIndex.map { case (line, idx) =>
//        idx -> line.replace("'", "")
//      }.toMap
//      source.close()
//      val ds = Ds("/home/davi/wcs/ucipp/uci", dataset)
//      ds.patterns foreach { p =>
//        //weka loader reindexes nominal attributes from zero (as in p.vector), but toString recovers original values
//        assertResult(arff(p.id).split(",").dropRight(1).mkString(",").take(50), s"$dataset id:${p.id}")(p.toString.split(",").dropRight(1).mkString(",").take(50))
//      }
//      ds.close()
//    }
//  }
//
//  "patterns' ids" should "survive to bina+zscore weka filters" in {
//    //label sequence in all datasets can be used to verify correctness of wekafiltered id sequence
//    datasets foreach { dataset =>
//      val ds = Ds("/home/davi/wcs/ucipp/uci", dataset)
//      val m = ds.patterns.map(p => p.id -> p.label).toMap
//      val shuffled = new Random(0).shuffle(ds.patterns)
//      val bf = Datasets.binarizeFilter(shuffled.take(30))
//      val bPatts = Datasets.applyFilter(bf)(shuffled.drop(3))
//      val zf = Datasets.zscoreFilter(bPatts)
//      val zbPatts = Datasets.applyFilter(zf)(bPatts)
//      zbPatts foreach { p =>
//        assertResult(m(p.id), s"$dataset id:${p.id}")(p.label)
//      }
//      ds.close()
//    }
//  }
//
//  "weights" should "remain 1 at input and output of filters" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    val zf = Datasets.zscoreFilter(bPatts)
//    val zbPatts = Datasets.applyFilter(zf)(bPatts)
//    ds.close()
//    assert(ds.patterns ++ bPatts ++ zbPatts forall (_.weight() == 1))
//  }
//  it should "raise Error if are not 1 before filters" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    val zf = Datasets.zscoreFilter(bPatts)
//    val zbPatts = Datasets.applyFilter(zf)(bPatts)
//    ds.close()
//    zbPatts.head.setWeight(0.1)
//    intercept[Error] {
//      Datasets.applyFilter(bf)(zbPatts)
//    }
//  }
//
//  "5-fold CV" should "create different folds" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    val zf = Datasets.zscoreFilter(bPatts)
//    ds.close()
//    val trs = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => tr}.toVector
//    assert(trs.map(_.sortBy(_.id)).distinct.size === trs.size)
//  }
//  it should "have 1 occurrence of each instance at 4 pools" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    val zf = Datasets.zscoreFilter(bPatts)
//    ds.close()
//    val trs = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => tr}.toVector
//    val occs = ds.patterns map { p =>
//      trs.map { tr => tr.count(_ == p)}
//    }
//    assert(occs.map(_.sorted) === Vector.fill(ds.patterns.size)(Vector(0, 1, 1, 1, 1)))
//  }
//  it should "have 1 occurrence of each instance for all ts folds" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    ds.close()
//    val tss = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => ts}.toVector
//    val occs = ds.patterns map { p =>
//      tss.map { ts => ts.count(_ == p)}
//    }
//    assert(occs.map(_.sorted) === Vector.fill(ds.patterns.size)(Vector(0, 0, 0, 0, 1)))
//  }
//  it should "have no instance in both pool and ts" in {
//    val ds = Ds("/home/davi/wcs/als/", "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    ds.close()
//    val trs = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => tr}.toVector
//    val tss = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => ts}.toVector
//    trs.zip(tss) foreach { case (tr, ts) =>
//      assert(ts.intersect(tr).isEmpty)
//    }
//  }
//  it should "not miss any instance" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    ds.close()
//    val tss = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => ts}.toVector
//    assert(ds.patterns.diff(tss.flatten).isEmpty)
//  }
//  it should "have pools with size not exceeding min+1" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    ds.close()
//    val trs = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => tr}.toVector
//    val min = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => min}.head
//    trs foreach (tr => assert(tr.size === min || tr.size === min + 1))
//  }
//  it should "have tss with 0.2 the original size" in {
//    val ds = Ds(Global.appPath, "flags-colour")
//    ds.open()
//    val shuffled = new Random(0).shuffle(ds.patterns)
//    val bf = Datasets.binarizeFilter(shuffled.take(30))
//    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
//    ds.close()
//    val tss = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => ts}.toVector
//    tss foreach (ts => assert(ts.size === (ds.patterns.size * 0.2).toInt || ts.size === (ds.patterns.size * 0.2).toInt + 1))
//  }
//}