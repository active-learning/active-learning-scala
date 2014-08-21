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

import al.strategies.Strategy
import app.ArgParser
import app.db.entities.Database
import ml.classifiers.Learner

case class AppFile(createOnAbsence: Boolean = false, readOnly: Boolean = false) extends Database {
  println("App. path = " + ArgParser.appPath)
  val database = "app"
  val path = ArgParser.appPath

  def createOtherTables() {
    if (readOnly) {
      println("Cannot create tables on a readOnly database!")
      sys.exit(1)
    }
    if (connection == null) {
      println("Impossible to get connection to create other tables. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(1)
    }

    try {
      val statement = connection.createStatement()
      statement.executeUpdate("begin")
      statement.executeUpdate("create table medida ( name VARCHAR, unique (name) on conflict rollback)")
      val measures = Seq("Q", "ALCDaAcc", "ALCDaGmeans", "custoPraAccPass", "custoPraGmeansPass", "accEmQ", "gmeansEmQ", "tempoDaPiorConsulta", "tempoMedioDeConsulta", "tempoPraQ")
      measures.foreach(med => statement.executeUpdate(s"insert into table medida values ('$med')"))

      //todo: useless tables?
      statement.executeUpdate("create table path ( name VARCHAR, desc VARCHAR, unique (name) on conflict rollback)")
      statement.executeUpdate("create table dataset ( name VARCHAR, pathid INT, unique (name) on conflict rollback)")
      statement.executeUpdate("create table meta ( datasetid INT, name VARCHAR, value FLOAT, unique (datasetid, name) on conflict rollback)")
      statement.executeUpdate("create table config ( name VARCHAR, value FLOAT, unique (name) on conflict rollback)")

      statement.executeUpdate("end")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems creating other tables in: " + dbCopy + ":")
        println(e.getMessage)
        println("Deleting " + dbCopy + "...")
        dbCopy.delete()
        println(" " + dbCopy + " deleted!")
        sys.exit(1)
    }
    println("Other tables created in " + dbCopy + ".")
  }

  def createTableOfLearners(learners: Seq[Learner]) {
    if (readOnly) {
      println("Cannot create tables on a readOnly database!")
      sys.exit(1)
    }
    if (connection == null) {
      println("Impossible to get connection to write " + learners.length + " learners. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(1)
    }

    //Insert all learners' names.
    try {
      val statement = connection.createStatement()
      statement.executeUpdate("begin")
      statement.executeUpdate("create table learner ( name VARCHAR, unique (name) on conflict rollback)")
      learners.zipWithIndex.foreach { case (learner, idx) => statement.executeUpdate(s"insert into learner values ('$learner')")}
      statement.executeUpdate("end")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems inserting learners into: " + dbCopy + ":")
        println(e.getMessage)
        println("Deleting " + dbCopy + "...")
        dbCopy.delete()
        println(" " + dbCopy + " deleted!")
        sys.exit(1)
    }
    println(learners.length + " learners written to " + dbCopy + ".")
  }

  def createTableOfStrategies(strats: Seq[Strategy]) {
    if (readOnly) {
      println("Cannot create tables on a readOnly database!")
      sys.exit(1)
    }
    if (connection == null) {
      println("Impossible to get connection to write " + strats.length + " strategies. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(1)
    }

    //Insert all strategies' names.
    try {
      val statement = connection.createStatement()
      statement.executeUpdate("begin")
      statement.executeUpdate("create table strategy ( name VARCHAR, unique (name) on conflict rollback)")
      strats.zipWithIndex.foreach { case (strat, idx) =>
        statement.executeUpdate(s"insert into strategy values ('$strat')")
      }
      statement.executeUpdate("end")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems inserting strategies into: " + dbCopy + ":")
        println(e.getMessage)
        println("Deleting " + dbCopy + "...")
        dbCopy.delete()
        println(" " + dbCopy + " deleted!")
        sys.exit(1)
    }
    println(strats.length + " strategies written to " + dbCopy + ".")
  }
}

