package novo

import al.strategies._
import ml.Pattern
import ml.classifiers.Learner

import scala.io.Source

trait Args extends App {
  private lazy val argsb = args filter (x => x.endsWith("=y") || x.endsWith("=n"))
  private lazy val argsn = args filter (x => x.split('=').last.filter(x => x != '.' && x != '-').forall(x => x.isDigit))
  private lazy val argsl = args filter (x => x.contains(",") || x.startsWith("file=") || x.startsWith("datasets=") || x.startsWith("neigs="))
  private lazy val argst = args diff argsb diff argsn diff argsl

  lazy val argb = {
//    val tmp =
      (argsb map parse map (x => x._1 -> x._2.equals("y"))).toMap
//    val tmp2 = if (tmp.contains("dry")) tmp else Map("dry" -> false) ++ tmp
//    if (tmp2.contains("clear")) tmp2 else Map("clear" -> false) ++ tmp2
  }
  lazy val argi = argsn map parse map (x => x._1 -> x._2.toInt) toMap
  lazy val argl = argsl map parse map {
    case ("file", file) => "file" -> Source.fromFile(file).getLines().toList
    case ("neigs", n) if !n.contains(",") => "neigs" -> List(n)
    case (k, v) => k -> v.split(',').toList
  } toMap
  lazy val argt = argst map parse toMap

  def parse(s: String) = {
    val Seq(a, b) = s.split('=').toSeq
    a -> b
  }

  def strat(pool: Seq[Pattern], l: Learner) = argt("strat") match {
    case "?" => println("Strats: rnd, mar, tu, eer, hs, sg"); sys.exit
    case "tu" => TU(pool, l, pool)
    case "mar" => Mar(l, pool)
    case "hs" => HS(pool)
    case "eer" => EER(l, pool, "entropy")
    case "sg" => SG(l, pool, "consensus")
    case "rnd" => Rnd(pool)
    case x => sys.error(s"Strategy $x not reconized.")
  }
}
