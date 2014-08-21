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
import app.db.entities.{Dataset, AppFile}

/**
 * Created by davi on 09/06/14.
 */
object InsertDatasetNames extends App {
  val desc = "Version " + ArgParser.version + " \n Insert given path and dataset names in app.db."
  val (path, datasetNames, pathName) = ArgParser.testArgsWithTextNoPar(getClass.getSimpleName.dropRight(1), args, desc)
  val dest = Dataset(path) _

  val af = AppFile()
  af.open()
  af.exec(s"begin")
  af.exec(s"insert into path values ('$pathName', '$path')")
  val pid = af.exec(s"select rowid from path where name='$pathName'").get.head.head.toInt
  datasetNames foreach { datasetName =>
    af.exec(s"insert into dataset values ('$datasetName', $pid)")
  }
  af.exec(s"end")
  af.save()
  af.close()
}
