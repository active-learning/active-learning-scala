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

import java.sql.{Connection, DriverManager}
import org.apache.commons.io.FileUtils
import java.io.File
import al.strategies.Strategy
import app.ArgParser
import ml.classifiers.Learner

case class AppFile() extends Database {
  println("App. path = " + ArgParser.appPath)
  val database = "app"
  val path = ArgParser.appPath

  def createOtherTables() {
    if (connection == null) {
      println("Impossible to get connection to create other tables. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(0)
    }

    try {
      val statement = connection.createStatement()
      statement.executeUpdate("begin")
      statement.executeUpdate("create table medida ( name VARCHAR, unique (name) )")
      statement.executeUpdate("create table path ( name VARCHAR, desc VARCHAR, unique (name) )")
      statement.executeUpdate("create table dataset ( name VARCHAR, path INT, unique (name) )")
      statement.executeUpdate("create table meta ( dataset INT, name VARCHAR, value FLOAT, unique (dataset, name) )")
      statement.executeUpdate("create table config ( name VARCHAR, value FLOAT, unique (name) )")
      statement.executeUpdate("end")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems creating other tables in: " + dbCopy + ":")
        println(e.getMessage)
        println("Deleting " + dbCopy + "...")
        dbCopy.delete()
        println(" " + dbCopy + " deleted!")
        sys.exit(0)
    }
    println("Other tables created in " + dbCopy + ".")
  }

  def createTableOfLearners(learners: Seq[Learner]) {
    if (connection == null) {
      println("Impossible to get connection to write " + learners.length + " learners. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(0)
    }

    //Insert all learners' names.
    try {
      val statement = connection.createStatement()
      statement.executeUpdate("begin")
      statement.executeUpdate("create table learner ( name VARCHAR, unique (name) )")
      learners.zipWithIndex.foreach { case (learner, idx) => statement.executeUpdate("insert into learner values ('" + learner + "')")}
      statement.executeUpdate("end")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems inserting queries into: " + dbCopy + ":")
        println(e.getMessage)
        println("Deleting " + dbCopy + "...")
        dbCopy.delete()
        println(" " + dbCopy + " deleted!")
        sys.exit(0)
    }
    println(learners.length + " queries written to " + dbCopy + ".")
  }

  def createTableOfStrategies(strats: Seq[Strategy]) {
    if (connection == null) {
      println("Impossible to get connection to write " + strats.length + " strategies. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(0)
    }

    //Insert all strategies' names.
    try {
      val statement = connection.createStatement()
      statement.executeUpdate("begin")
      statement.executeUpdate("create table strategy ( name VARCHAR, learner INT, unique (name, learner) )")
      strats.zipWithIndex.foreach { case (strat, idx) =>

        //Fetch LearnerId by name.
        var learnerId = -1
        try {
          val statement0 = connection.createStatement()
          val resultSet0 = statement0.executeQuery("select rowid from learner where name='" + strat.learner + "'")
          println("select rowid from learner where name='" + strat.learner + "'")
          resultSet0.next()
          learnerId = resultSet0.getInt("rowid")
        } catch {
          case e: Throwable => e.printStackTrace
            println("\nProblems inserting queries into: " + dbCopy + ".")
            sys.exit(0)
        }

        statement.executeUpdate("insert into strategy values ('" + strat + "', " + learnerId + ")")
      }
      statement.executeUpdate("end")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems inserting queries into: " + dbCopy + ":")
        println(e.getMessage)
        println("Deleting " + dbCopy + "...")
        dbCopy.delete()
        println(" " + dbCopy + " deleted!")
        sys.exit(0)
    }
    println(strats.length + " queries written to " + dbCopy + ".")
  }
}

