import clean.{Ds, Db}
import util.Datasets

import scala.io.Source
import scala.util.Random

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

class MySpec extends UnitSpec {
  val ds = Ds("/home/davi/wcs/ucipp/uci")("flags-colour")
  val shuffled = new Random(0).shuffle(ds.patterns)
  val bf = Datasets.binarizeFilter(shuffled.take(30))
  val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))
  val zf = Datasets.zscoreFilter(bPatts)
  val zbPatts = Datasets.applyFilter(zf)(bPatts)
  ds.close()

  "Database" should "create a table, write and read two tuples" in {
    val db = new Db("/home/davi/wcs/als/test.db")
    assert(db.write("drop table if exists test") ===())
    assert(db.write("create table test (a INT, b FLOAT)") ===())
    assert(db.write("insert into test values (7, 0.7)") ===())
    assert(db.write("insert into test values (8, 0.8)") ===())
    assertResult(List(Vector(7, 0.7), Vector(8, 0.8)))(db.read("select * from test"))
    db.close()
  }

  "Dataset db file" should "have ids matching ARFF line numbers" in {
    val source = Source.fromFile("/home/davi/wcs/ucipp/uci/flags-colour.arff")
    val arff = source.getLines().dropWhile(!_.contains("@data")).toList.tail.zipWithIndex.map { case (line, idx) =>
      idx -> line.replace("'", "")
    }.toMap
    source.close()
    ds.patterns foreach { p =>
      assertResult(arff(p.id))(p.toString) //weka loader reindexes nominal attributes from zero (as in p.vector), but toString recovers original values
    }
  }

  "patterns' ids" should "survive to bina+zscore weka filters" in {
    //label sequence in 'flags' dataset can be used to verify correctness of wekafiltered id senquence
    val m = ds.patterns.map(p => p.id -> p.label).toMap
    zbPatts foreach { p =>
      assertResult(m(p.id))(p.label)
    }
  }

  "weights" should "remain 1 at input and output of filters" in {
    assert(ds.patterns ++ bPatts ++ zbPatts forall (_.weight() == 1))
  }
  it should "raise Error if are not 1 before filters" in {
    zbPatts.head.setWeight(0.1)
    intercept[Error] {
      Datasets.applyFilter(bf)(zbPatts)
    }
  }

  val trs = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => tr}.toVector
  val tss = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => ts}.toVector
  val min = Datasets.kfoldCV(ds.patterns, 5) { (tr, ts, fold, min) => min}.head
  "5-fold CV" should "create different folds" in {
    assert(trs.map(_.sortBy(_.id)).distinct.size === trs.size)
  }
  it should "have 1 occurrence of each instance at 4 pools" in {
    val occs = ds.patterns map { p =>
      trs.map { tr => tr.count(_ == p)}
    }
    assert(occs.map(_.sorted) === Vector.fill(ds.patterns.size)(Vector(0, 1, 1, 1, 1)))
  }
  it should "have 1 occurrence of each instance for all ts folds" in {
    val occs = ds.patterns map { p =>
      tss.map { ts => ts.count(_ == p)}
    }
    assert(occs.map(_.sorted) === Vector.fill(ds.patterns.size)(Vector(0, 0, 0, 0, 1)))
  }
  it should "have no instance in both pool and ts" in {
    trs.zip(tss) foreach { case (tr, ts) =>
      assert(ts.intersect(tr).isEmpty)
    }
  }
  it should "not miss any instance" in {
    assert(ds.patterns.diff(tss.flatten).isEmpty)
  }
  it should "have pools with size not exceeding min+1" in {
    trs foreach (tr => assert(tr.size === min || tr.size === min + 1))
  }
  it should "have tss with 0.2 the original size" in {
    tss foreach (ts => assert(ts.size === (ds.patterns.size * 0.2).toInt || ts.size === (ds.patterns.size * 0.2).toInt + 1))
  }
}