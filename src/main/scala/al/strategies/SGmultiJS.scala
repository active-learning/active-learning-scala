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

import java.awt.Color

import ml.Pattern
import ml.classifiers.Learner
import ml.models.Model
import util.Graphics
import weka.classifiers.trees.JSMeasure

case class SGmultiJS(learner: Learner, pool: Seq[Pattern], debug: Boolean = false)
  extends StrategySGmulti with JSMeasure {
  override val toString = "SGmultiJS"
  val abr = "SGJS"
  val id = 1616

  def controversial(unlabeled: Seq[Pattern], current_models: Array[Model]) = unlabeled maxBy (pa => JSdivergence(current_models.map(m => m.distribution(pa))))

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
        val js0 = 1 - normalizedJSdivergence(models_to_visualize.map(m => m.distribution(p))).toFloat
        val cor = if (js0 >= 0.8) {
          Graphics.n2cor(models_to_visualize.map(_.distribution(p)).transpose.map(_.sum).zipWithIndex.max._2)
        } else new Color(js0, js0, js0)
        plot.bola_color(p.x, p.y, cor, 9)
      }
      for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
      plot.mostra()
    }
    Thread.sleep((delay * 1000).round.toInt)
  }
}