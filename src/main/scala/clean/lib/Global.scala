package clean.lib

import java.io.File

import scala.io.Source

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
object Global {
   //17176,17177,17178,17179 e 4173006,4173007,4173008,4173009 são gno com learner interno, mas recebem tratamento de agnósticas
   //9660091, 9670092, 9680093, 9690094 passaram pro outro grupo para permitir RF no hits, por exemplo
   val gnosticasComLearnerInterno = Seq(1292212, 1006600, 10066, 966000, 967000, 968000, 969000, 966009, 967009, 968009, 969009)

   val agnosticasa = Seq(211, 691, 361, 66361, 991, 391, 66391, 791, 371, 599996, 599997, 599998, 599999,

      127176, 127177, 127178, 127179, 94172006, 94172007, 94172008, 94172009,
      127186, 127187, 127188, 127189, 94172016, 94172017, 94172018, 94172019,
      127196, 127197, 127198, 127199, 94172026, 94172027, 94172028, 94172029,
      127206, 127207, 127208, 127209, 94172036, 94172037, 94172038, 94172039,
      127216, 127217, 127218, 127219, 94172046, 94172047, 94172048, 94172049,
      127226, 127227, 127228, 127229, 94172056, 94172057, 94172058, 94172059,
      3000000, 3000010, 3000020, 3000030, 3000040, 3000050,
      4000000, 4000010, 4000020, 4000030, 4000040, 4000050,
      11000000, 11000010, 11000020, 11000030, 11000040, 11000050,
      14000000, 14000010, 14000020, 14000030, 14000040, 14000050,
      9660091, 9670092, 9680093, 9690094,
      77000000, 77000010, 77000020, 77000030, 77000040, 77000050,
      78000000, 78000010, 78000020, 78000030, 78000040, 78000050,
      79000000, 79000010, 79000020, 79000030, 79000040, 79000050,
      80000000, 80000010, 80000020, 80000030, 80000040, 80000050,
      81000000, 81000010, 81000020, 81000030, 81000040, 81000050,
      //coloquei mar, sg e eer aqui porque suas queries vão ser identicas às antigas
      3,
      11, 12, 13, 74,
      14, 15
      , 3292212
   )

   lazy val (mysqlHostRW, mysqlPortRW, mysqlPassRW) = {
     val l = Source.fromFile("mysql.txt").getLines().toList
      (l(0), l(1), l(2))
   }
   lazy val (mysqlHostRO, mysqlPortRO, mysqlPassRO) = {
     val l = Source.fromFile("mysql.txt").getLines().toList
      (l(3), l(4), l(5))
   }
   var running = true
   lazy val memlimit = Source.fromFile("memlimit.txt").getLines().toList.head.toInt
   var debug = 20
   lazy val runs = Source.fromFile("runs.txt").getLines().toList.head.toInt
   lazy val folds = Source.fromFile("folds.txt").getLines().toList.head.toInt
   val appPath = new File(".").getCanonicalPath + "/"

   def mysqlHost(readOnly: Boolean) = if (readOnly) mysqlHostRO else mysqlHostRW

   def mysqlPort(readOnly: Boolean) = if (readOnly) mysqlPortRO else mysqlPortRW

   def mysqlPass(readOnly: Boolean) = if (readOnly) mysqlPassRO else mysqlPassRW

}
