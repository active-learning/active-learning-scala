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
  "A Db" should "create a table, write and read two tuples" in {
    val db = new Db("/home/davi/wcs/als/test.db")
    assert(db.write("drop table if exists test") ===())
    assert(db.write("create table test (a INT, b FLOAT)") ===())
    assert(db.write("insert into test values (7, 0.7)") ===())
    assert(db.write("insert into test values (8, 0.8)") ===())
    assertResult(List(Vector(7, 0.7), Vector(8, 0.8)))(db.read("select * from test"))
    db.close()
  }

  "A nonAttsProjected and nonUselessAtssRemoved dataset db file" should "have ids matching with the line numbers from ARFF file" in {
    val source = Source.fromFile("/home/davi/wcs/ucipp/uci/flags.arff")
    val arff = source.getLines().dropWhile(!_.contains("@data")).toList.tail.zipWithIndex.map { case (line, idx) =>
      idx -> line.replace("'", "")
    }.toMap
    source.close()
    val ds = Ds("/home/davi/wcs/ucipp/uci")("flags")
    ds.patterns foreach { p =>
      assertResult(arff(p.id))(p.toString) //weka loader reindexes nominal attributes from zero (as in p.vector), but toString recovers original values
    }
    ds.close()
  }

  "id of each pattern" should "survive to bina+zscore weka filters" in {
    //label sequence of flags dataset can be used to verify correctness of id senquence
    val ds = Ds("/home/davi/wcs/ucipp/uci")("flags")
    val shuffled = new Random(0).shuffle(ds.patterns)
    val bf = Datasets.binarizeFilter(shuffled.take(30))
    val bPatts = Datasets.applyFilter(bf)(shuffled.drop(30))

    val zf = Datasets.zscoreFilter(bPatts)
    val zbPatts = Datasets.applyFilter(zf)(bPatts)

    val m = ds.patterns.map(p => p.id -> p.label).toMap

    zbPatts foreach { p =>
      assertResult(m(p.id))(p.label)
    }
    ds.close()
  }


  //  it should "throw NoSuchElementException if an empty stack is popped" in {
  //    val emptyStack = new Stack[String]
  //    intercept[NoSuchElementException] {
  //      emptyStack.pop()
  //    }
  //  }
}