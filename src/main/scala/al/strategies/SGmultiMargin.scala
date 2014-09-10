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

package al.strategies

import ml.Pattern
import ml.classifiers.Learner
import java.awt.Color
import ml.models.Model
import util.Graphics

case class SGmultiMargin(learner: Learner, pool: Seq[Pattern], debug: Boolean = false)
  extends StrategySGmulti with EntropyMeasure with MarginMeasure {
  override val toString = "SGmultiMargin"
  val abr = "SGmar"
  val id = -303

  def controversial(unlabeled: Seq[Pattern], current_models: Array[Model]) =
    unlabeled minBy {
      x => val combined_distribution = current_models.map(m => m.distribution(x)).transpose.map(_.sum)
        margin0(combined_distribution)
    }

  def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {

    if (selected != null) {
      for ((plot, i) <- plots.zipWithIndex) {
        plot.zera()
        for (p <- distinct_pool) plot.bola(p.x, p.y, models_to_visualize(i).predict(p).toInt, 9)
        for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
        plot.bola(selected.x, selected.y, -1, 15)
        plot.mostra()
      }
      plot.bola(selected.x, selected.y, -1, 15)
      plot.mostra()
    } else {
      plot.zera()
      if (models_to_visualize.head != null) for (p <- distinct_pool) {
        val combined_distribution = models_to_visualize.map(m => m.distribution(p)).transpose.map(_.sum)
        val js0 = margin0(combined_distribution).toFloat
        val cor = if (js0 > 0.7) {
          Graphics.n2cor(models_to_visualize.head.predict(p).toInt)
        } else new Color(js0, js0, js0)
        plot.bola_color(p.x, p.y, cor, 9)
      }
      for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
      plot.mostra()
    }
    Thread.sleep((delay * 1000).round.toInt)
  }
}