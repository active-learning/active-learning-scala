package novo

import clean.lib.{CM, Ds, Global}
import ml.classifiers.{Maj, RF}
import util.{Datasets, Stat}

import scala.io.Source
import scala.util.Random

trait Args extends App {
  private lazy val argsb = args filter (x => x.endsWith("=y") || x.endsWith("=n"))
  private lazy val argsn = args filter (x => x.split('=').last.filter(x => x != '.' && x != '-' ).forall(x => x.isDigit))
  private lazy val argsl = args filter (x => x.contains(",") || x.startsWith("file="))

  lazy val argb = argsb map parse map (x => x._1 -> x._2.equals("y")) toMap
  lazy val argi = argsn map parse map (x => x._1 -> x._2.toInt) toMap
  lazy val argl = argsl map parse map {
    case ("file", file) => "file" -> Source.fromFile(file).getLines().toList
    case (k, v) => k -> v.split(',').toList
  } toMap

  def parse(s: String) = {
    val Seq(a, b) = s.split('=').toSeq
    a -> b
  }
}
