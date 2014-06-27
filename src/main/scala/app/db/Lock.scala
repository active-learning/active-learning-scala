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

import java.io.File
import java.sql.{Connection, DriverManager}

import al.strategies.{RandomSampling, Strategy}
import app.ArgParser
import org.apache.commons.io.FileUtils
import util.{ALDatasets, Datasets}

import scala.collection.mutable
import scala.util.Random

trait Lock {
  private val rnd = new Random(10)
  private var available = true

  def acquire() = {
    Thread.sleep((rnd.nextDouble() * 100).toInt)
    synchronized {
      while (!available) wait()
      available = false
    }
  }

  def release() = {
    Thread.sleep((rnd.nextDouble() * 100).toInt)
    synchronized {
      available = true
      notify()
    }

  }
}

