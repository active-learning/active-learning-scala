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

package util

object Stat {
  def media_desvioPadrao(items: Vector[Double]) = {
    val s = items.sum
    val l = items.length.toDouble
    val m = s / l
    val v0 = (items map {
      x =>
        val di = x - m
        di * di
    }).sum / (l - 1)
    val v = if (v0.isNaN) 0 else v0
    val d = math.sqrt(v)
    (m, d)
  }

  def media_desvioPadraoi(items: Vector[Int]) = {
    val s = items.sum
    val l = items.length.toDouble
    val m = s / l
    val v0 = (items map {
      x =>
        val di = x - m
        di * di
    }).sum / (l - 1)
    val v = if (v0.isNaN) 0 else v0
    val d = math.sqrt(v)
    (m, d)
  }

  def media_intervalo_confianca90(items: Vector[Double]) = {
    val s = items.sum
    val l = items.length.toDouble
    val m = s / l
    val v0 = (items map {
      x =>
        val di = x - m
        di * di
    }).sum / (l - 1)
    val v = if (v0.isNaN) 0 else v0
    val d = math.sqrt(v)
    (m, 1.64 * d / math.sqrt(l)) //1.64  1.96
  }

  def media_intervalo_confianca95(items: Vector[Double]) = {
    val s = items.sum
    val l = items.length.toDouble
    val m = s / l
    val v0 = (items map {
      x =>
        val di = x - m
        di * di
    }).sum / (l - 1)
    val v = if (v0.isNaN) 0 else v0
    val d = math.sqrt(v)
    (m, 1.96 * d / math.sqrt(l)) //1.64  1.96
  }

  def media_std_intervalo_confianca99(items: Vector[Double]) = {
    val s = items.sum
    val l = items.length.toDouble
    val m = s / l
    val v0 = (items map {
      x =>
        val di = x - m
        di * di
    }).sum / (l - 1)
    val v = if (v0.isNaN) 0 else v0
    val d = math.sqrt(v)
    (m, d, 2.58 * d / math.sqrt(l)) //1.64  1.96  2.58
  }

  def hasTies(rank: List[Double]) = (rank.length > rank.distinct.length)

  val limitToTie = 0.005d

  /**
   * old code
   * @param l
   * @return
   */
  def wilcoxon(l: Seq[((String, Double), (String, Double))]) = {
    val temp = 1d / limitToTie
    def arredonda(a: Double) = (a * temp).round / temp

    val difs0 = l map {
      case ((nome, med), (nome2, med2)) =>
        (arredonda(med2 - med), arredonda((med2 - med).abs))
    }
    val difs = difs0.sortBy(_._2) //.filterNot(_==0)
    val distintos = difs.distinct
    val histo = distintos map { x =>
      (x, difs.count(_ == x))
    }
    val colocacoes0 = 1 to difs.length
    var colocacoes = colocacoes0.toList

    val histocomlistas = histo map {
      case (x, n) =>
        val r = (x, colocacoes.take(n))
        colocacoes = colocacoes.drop(n)
        r
    }

    val winrankhisto = histocomlistas map {
      case (x, l) =>
        (x, l.reduceLeft(_ + _).toDouble / l.length.toDouble)
    }

    val winrank = difs map {
      case (va, ab) =>
        val bla = winrankhisto.filter {
          case ((v, a), r) =>
            v == va
        }.head
        (bla._1._1, bla._2)
    }

    val positivos = winrank.filter {
      case (v, r) =>
        v > 0
    }

    val negativos = winrank.filter {
      case (v, r) =>
        v < 0
    }

    val zeros = winrank.filter {
      case (v, r) =>
        v == 0
    }

    val somap = if (positivos.length == 0) 0 else positivos.map(_._2).reduceLeft(_ + _)
    val soman = if (negativos.length == 0) 0 else negativos.map(_._2).reduceLeft(_ + _)
    val somaz = if (zeros.length == 0) 0 else zeros.map(_._2).reduceLeft(_ + _)

    var sinal = ""
    val R = if (somap >= soman) {
      sinal = "+"
      soman + (somaz / 2).toInt //dispensa excedente
    } else {
      sinal = "-"
      somap + (somaz / 2).toInt
    }

    val linhaswtable = List(
      (4, List((2, 0.5), (0, 0.2))),
      (5, List((4, 0.5), (2, 0.2), (0, 0.1))),

      (6, List((6, 0.5), (3, 0.2), (2, 0.1), (0, 0.05))),
      (7, List((9, 0.5), (5, 0.2), (3, 0.1), (2, 0.05), (0, 0.02))),
      (8, List((12, 0.5), (8, 0.2), (5, 0.1), (3, 0.05), (1, 0.02), (0, 0.01))),
      (9, List((16, 0.5), (10, 0.2), (8, 0.1), (5, 0.05), (3, 0.02), (1, 0.01), (0, 0.005))),
      (10, List((20, 0.5), (14, 0.2), (10, 0.1), (8, 0.05), (5, 0.02), (3, 0.01), (1, 0.005), (0, 0.001))),

      (11, List((24, 0.5), (17, 0.2), (13, 0.1), (10, 0.05), (7, 0.02), (5, 0.01), (3, 0.005), (0, 0.001))),
      (12, List((29, 0.5), (21, 0.2), (17, 0.1), (13, 0.05), (9, 0.02), (7, 0.01), (5, 0.005), (1, 0.001))),
      (13, List((35, 0.5), (26, 0.2), (21, 0.1), (17, 0.05), (12, 0.02), (9, 0.01), (7, 0.005), (2, 0.001))),
      (14, List((40, 0.5), (31, 0.2), (25, 0.1), (21, 0.05), (15, 0.02), (12, 0.01), (9, 0.005), (4, 0.001))),
      (15, List((47, 0.5), (36, 0.2), (30, 0.1), (25, 0.05), (19, 0.02), (15, 0.01), (12, 0.005), (6, 0.001))),

      (16, List((54, 0.5), (42, 0.2), (35, 0.1), (29, 0.05), (23, 0.02), (19, 0.01), (15, 0.005), (8, 0.001))),
      (17, List((61, 0.5), (48, 0.2), (41, 0.1), (34, 0.05), (27, 0.02), (23, 0.01), (19, 0.005), (11, 0.001))),
      (18, List((69, 0.5), (55, 0.2), (47, 0.1), (40, 0.05), (32, 0.02), (27, 0.01), (23, 0.005), (14, 0.001))),
      (19, List((77, 0.5), (62, 0.2), (53, 0.1), (46, 0.05), (37, 0.02), (32, 0.01), (27, 0.005), (18, 0.001))),
      (20, List((86, 0.5), (69, 0.2), (60, 0.1), (52, 0.05), (43, 0.02), (37, 0.01), (32, 0.005), (21, 0.001))),

      (21, List((95, 0.5), (77, 0.2), (67, 0.1), (58, 0.05), (49, 0.02), (42, 0.01), (37, 0.005), (25, 0.001))),
      (22, List((104, 0.5), (86, 0.2), (75, 0.1), (65, 0.05), (55, 0.02), (48, 0.01), (42, 0.005), (30, 0.001))),
      (23, List((114, 0.5), (94, 0.2), (83, 0.1), (73, 0.05), (62, 0.02), (54, 0.01), (48, 0.005), (35, 0.001))),
      (24, List((125, 0.5), (104, 0.2), (91, 0.1), (81, 0.05), (69, 0.02), (61, 0.01), (54, 0.005), (40, 0.001))),
      (25, List((136, 0.5), (113, 0.2), (100, 0.1), (89, 0.05), (76, 0.02), (68, 0.01), (60, 0.005), (45, 0.001))),

      (26, List((148, 0.5), (124, 0.2), (110, 0.1), (98, 0.05), (84, 0.02), (75, 0.01), (67, 0.005), (51, 0.001))),
      (27, List((160, 0.5), (134, 0.2), (119, 0.1), (107, 0.05), (92, 0.02), (83, 0.01), (74, 0.005), (57, 0.001))),
      (28, List((172, 0.5), (145, 0.2), (130, 0.1), (116, 0.05), (101, 0.02), (91, 0.01), (82, 0.005), (64, 0.001))),
      (29, List((185, 0.5), (157, 0.2), (140, 0.1), (126, 0.05), (110, 0.02), (100, 0.01), (90, 0.005), (71, 0.001))),
      (30, List((198, 0.5), (169, 0.2), (151, 0.1), (137, 0.05), (120, 0.02), (109, 0.01), (98, 0.005), (78, 0.001))),

      (31, List((212, 0.5), (181, 0.2), (163, 0.1), (147, 0.05), (130, 0.02), (118, 0.01), (107, 0.005), (86, 0.001))),
      (32, List((226, 0.5), (194, 0.2), (175, 0.1), (159, 0.05), (140, 0.02), (128, 0.01), (116, 0.005), (94, 0.001))),
      (33, List((241, 0.5), (207, 0.2), (187, 0.1), (170, 0.05), (151, 0.02), (138, 0.01), (126, 0.005), (102, 0.001))),
      (34, List((257, 0.5), (221, 0.2), (200, 0.1), (182, 0.05), (162, 0.02), (148, 0.01), (136, 0.005), (111, 0.001))),
      (35, List((272, 0.5), (235, 0.2), (213, 0.1), (195, 0.05), (173, 0.02), (159, 0.01), (146, 0.005), (120, 0.001))),

      (36, List((289, 0.5), (250, 0.2), (227, 0.1), (208, 0.05), (185, 0.02), (171, 0.01), (157, 0.005), (130, 0.001))),
      (37, List((305, 0.5), (265, 0.2), (241, 0.1), (221, 0.05), (198, 0.02), (182, 0.01), (168, 0.005), (140, 0.001))),
      (38, List((323, 0.5), (281, 0.2), (256, 0.1), (235, 0.05), (211, 0.02), (194, 0.01), (180, 0.005), (150, 0.001))),
      (39, List((340, 0.5), (297, 0.2), (271, 0.1), (249, 0.05), (224, 0.02), (207, 0.01), (192, 0.005), (161, 0.001))) //,        (40, List((358,0.5),(313,0.2),(286,0.1),(264,0.05),(228,0.02),(,0.01),(,0.005),(,0.001))
    )

    val alfalist = linhaswtable.filter(_._1 == l.length).head._2

    val alfa = alfalist.filter {
      case (m, a) => R <= m
    }

    ("Para n=" + l.length + ":\n   R" + sinal + "=" + R + "\n   alfa:") + alfa.map(x => "\n      R" + sinal + " <= " + x._1 + ": " + x._2)
  }
}
