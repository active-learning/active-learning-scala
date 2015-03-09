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
   val gnosticasComLearnerInterno = Seq(1292212, 1006600, 10066, 966000, 967000, 968000, 969000, 966009, 967009, 968009, 969009, 3292212)

   val agnosticas = Seq(211, 601, 361, 66361, 901, 391, 66391, 701, 371, 599996, 599997, 599998, 599999)

   lazy val (mysqlHostRW, mysqlPortRW, mysqlPassRW) = {
      val l = Source.fromFile("/usr/local/share/mysql.txt").getLines().toList
      (l(0), l(1), l(2))
   }
   lazy val (mysqlHostRO, mysqlPortRO, mysqlPassRO) = {
      val l = Source.fromFile("/usr/local/share/mysql.txt").getLines().toList
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
