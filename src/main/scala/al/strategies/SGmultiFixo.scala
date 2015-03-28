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
import ml.classifiers._
import ml.models.Model

case class SGmultiFixo(learner: Learner, pool: Seq[Pattern], agreement: String, debug: Boolean = false)
   extends StrategySGmulti {
   override val toString = "SGmulti (" + learner.limpa + agreement + ")"
   val abr = "\\textbf{SGmulti}" + learner.limpa
   // + agreement.head + "}"
   //+ agreement.take(3)
   val id = agreement match {
      case "consensus" => 14000000 + convlid(learner.id)
      case "majority" => 15000000 + convlid(learner.id)
   }


   def controversial(unlabeled: Seq[Pattern], current_models: Array[Model]) =
      agreement match {
         case "consensus" => unlabeled find {
            pa =>
               val preds = current_models.map(mo => mo.predict(pa))
               preds.exists(_ != preds.head)
         } match {
            case Some(pattern) => pattern
            case None => unlabeled.head
         }
         case "majority" => unlabeled minBy {
            pa =>
               val preds = current_models.map(mo => mo.predict(pa).toInt)
               most_votes(preds)
         }
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
         if (models_to_visualize.head != null) for (p <- distinct_pool) plot.bola(p.x, p.y, if (models_to_visualize.map(mo => mo.predict(p).toInt).exists(_ != models_to_visualize(0).predict(p).toInt)) 15 else models_to_visualize(0).predict(p).toInt, 9)
         for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
         plot.mostra()
      }
      Thread.sleep((delay * 1000).round.toInt)
   }

}
