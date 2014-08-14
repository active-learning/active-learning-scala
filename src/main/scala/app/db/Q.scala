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

package app.db

import app.ArgParser

/**
 * Created by davi on 09/06/14.
 */
object Q extends App {
  val desc = s"Version ${ArgParser.version} \n Imprime Qs."
  val (path, datasetNames) = ArgParser.testArgs(getClass.getSimpleName.dropRight(1), args, 3, desc)
  val parallel = args(2) == "y"
  val readOnly = true
  val runs = 5
  val folds = 5
  val dest = Dataset(path, createOnAbsence = false, readOnly) _
  val qname = (if (parallel) datasetNames.par else datasetNames).toList map { datasetName =>
    val db = dest(datasetName)
    if (db.dbOriginal.exists()) {
      db.open()
      //Pega mediana.
      val Q = (for {
        r <- 0 until runs
        f <- 0 until folds
        sql = s"select p from (select run as r,fold as f,learnerid as l,strategyid as s,position as p,sum(value) as t from hit group by strategyid, learnerid, run, fold, position) inner join (select *,sum(value) as a from hit where expe=pred group by strategyid, learnerid, run, fold, position) on r=run and f=fold and s=strategyid and p=position and l=learnerid and r=$r and f=$f and s=1 and l=2 order by a/(t+0.0) desc, p asc limit 1;"
      } yield {
        db.exec(sql).get.head.head
      }).sorted.toList(runs * folds / 2).toInt
      db.close()
      (Q, datasetName)
    } else (-1, datasetName)
  }
  qname.sortBy(_._1) foreach println
  println("")
  println(qname.sortBy(_._1).map(_._2).mkString(","))
  println("")
}
