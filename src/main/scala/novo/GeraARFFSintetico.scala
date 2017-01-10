package novo

import java.io.FileWriter

import scala.util.Random

object GeraARFFSintetico extends App {
  val rnd = new Random(1324)

  def printuple[X](c: Int, w: Double)(tup: (X, X)) = tup._1 + ", " + tup._2 + ", " + c + s", {$w}\n"

  def disturb(s: Double)(x: Double, y: Double) = (s * rnd.nextDouble() + x - s / 2, s * rnd.nextDouble() + y - s / 2)

  def espiralAleatoria(x: Double, y: Double, n: Int) = {
    val ids = (1 to 10 * n by 5) map (x => math.sqrt(x.toDouble))
    val angs = ids map (_ * 1)
    val raios = ids map (_ * 0.1)
    angs.zip(raios) map { case (a, r) => disturb(math.sqrt(r) * 2)(x + r * math.cos(a), y + r * math.sin(a)) }
  }

  def uniforme(n: Int) = for {x <- -1000 to 1000 by n; y <- -1000 to 1000 by n} yield (x / 100d, y / 100d)

  val labeled0 = Seq[(Double, Double, Int)]((2.4, 2.5, 1), (2.3, 2.6, 1), (2.6, 2.7, 1), (2.7, 2.4, 1), (2.74, 2.85, 1), (2.84, 2.5, 1), (2.94, 2.45, 1), (2.84, 2.75, 1),
    (6, 6, 0), (4, 0, 0), (5, 4, 0), (-6, -4, 1), (-5, -5, 1), (-2, -3, 2), (-2, -6, 2), (-2.3, -3.3, 2), (-2.8, -6.2, 2),
    (6.2, 6.1, 0), (4.1, -0.2, 0), (5.2, 3.9, 0), (-5.8, -4.1, 1), (-5.05, -5.1, 1), (-2.1, -2.9, 2), (-2.2, -5.1, 2),
    (6.3, 6.4, 0), (4.3, -0.4, 0), (5.3, 3.4, 0), (5.5, 6, 0), (4.5, 0, 0), (5.5, 4, 0), (-6.5, -4, 1), (-5.5, -5, 1), (-2.5, -3, 2), (-2.5, -6, 2),
    (6.25, 6.4, 0), (4.11, -0.22, 0), (5.21, 3.92, 0), (-5.82, -4.17, 1), (-5.35, -5.01, 1), (-2.01, -2.09, 2), (-2.26, -5.51, 2),
    (6.53, 6.64, 0), (4.83, -0.94, 0), (5.13, 3.04, 0))
  val labeled = labeled0 ++ labeled0.map { case (a, b, c) => (a + Random.nextDouble(), b + Random.nextDouble(), 2) }
  val unlabeled = Seq[(Double, Double, Int)]((0.5, 4, 3), (-0.5, 4, 2), (-0.5, 5, 3),  (-1, 4.3, 2), (-0.5, 2.5, 5), (-1, 5, 2), (5, 5, 5), (7, 7, 5), (-3, 0, 5), (6, 0, 5), (-3.8, 3.5, 3), (-4.2, 2.5, 3), (3, 0, 10), (6, -4, 5), (3, 3, 10), (-6, -4, 5), (0, -4, 5), (0, 0, 10))

  Seq("unlabeled", "labeled", "ign", "repr", "fundo", "ent") foreach { set =>
    val fw = new FileWriter(s"/home/davi/git/als/$set.arff")
    fw.write(s"@relation synthetic-$set\n")
    fw.write("@attribute x numeric\n")
    fw.write("@attribute y numeric\n")
    fw.write("@attribute c {0,1,2,9,10,19,24,29,39,40,41,42,49}\n")
    fw.write("@data\n")


    set match {
      case "labeled" =>
        labeled foreach { case (x, y, c) => fw.write(printuple(c, 1)(x, y)) }
      case "unlabeled" =>
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(9, 1)(x))) }
      case "ign" =>
        labeled foreach { case (x, y, c) => fw.write(printuple(10, 1)(x, y)) }
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(19, 0.0000005)(x))) }
      case "repr" =>
        labeled foreach { case (x, y, c) => fw.write(printuple(24, 1)(x, y)) }
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(24, 1)(x))) }
        uniforme(50) foreach (x => fw.write(printuple(29, 0.00000001)(x)))
      case "fundo" =>
        uniforme(15) foreach (x => fw.write(printuple(39, 1)(x)))
      case "ent" => //mig
        labeled foreach { case (x, y, c) => fw.write(printuple(40 + c, 1)(x, y)) }
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(49, 0.001)(x))) }
    }
    fw.close()
  }
}
