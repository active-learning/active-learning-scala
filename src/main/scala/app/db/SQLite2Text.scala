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
import util.{ALDatasets, Datasets}

object SQLite2Text extends App {
  Class.forName("org.sqlite.JDBC")
  val desc = """ bla """

  val (path, names) = ArgParser.testArgs(getClass.getSimpleName.dropRight(1), args, 3, desc)
  names foreach { name =>
    ALDatasets.patternsFromSQLite(path)(name) match {
      case Right(patts) => patts foreach (x => println(x.id + "   " + x))
      case Left(str) => throw new Error(str)
    }
  }
}
