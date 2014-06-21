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

import java.awt.image.BufferedImage
import javax.swing.{WindowConstants, JFrame}
import java.awt.Color

object Graphics {

   def n2cor(n: Int): Color = {
      n match {
         case -1 => Color.red
         case 0 => new Color(0f, 0.4f, 0f)
         case 1 => new Color(0f, 0f, 0.95f)
         case 2 => Color.magenta
         case 3 => new Color(0.5f, 0f, 1f)
         case 4 => new Color(0.3f, 1f, 1f)
         case 5 => new Color(1f, 1f, 0f)
         case 6 => new Color(1f, 0f, 1f)
         case 7 => new Color(1f, 1f, 1f)
         case 8 => new Color(0, 0.5f, 1f)
         case 9 => new Color(1f, 0.5f, 0f)
         case 10 => new Color(0f, 1f, 0.5f)
         case 11 => new Color(1f, 0f, 0.5f)
         case 12 => new Color(0.5f, 1f, 0f)
         case 13 => Color.black
         case 14 => Color.lightGray
         case 15 => Color.darkGray
      }
   }

   class Plot {
      val w = 350
      val h = 350
      val bi = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
      val gr = bi.createGraphics
      val top = new JFrame()

      top.setSize(w, h)
      top.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      top.setVisible(true)


      def zera() {
         gr.setColor(Color.black)
         gr.setPaint(Color.black)
         gr.fillRect(0, 0, w, h)
      }

      def scalex(x: Double) = w / 2d + x * w / 2.1d

      def scaley(y: Double) = h / 2d - y * h / 2.1d

      def circ(x: Double, y: Double, cor: Int, diametro: Int) {
         gr.setColor(n2cor(cor))
         gr.setPaint(n2cor(cor))
         gr.drawOval(scalex(x).toInt - diametro / 2, scaley(y).toInt - diametro / 2, diametro, diametro)
      }

      def bola(x: Double, y: Double, cor: Int, diametro: Int) {
         gr.setColor(n2cor(cor))
         gr.setPaint(n2cor(cor))
         gr.fillOval(scalex(x).toInt - diametro / 2, scaley(y).toInt - diametro / 2, diametro, diametro)
      }

      def bola_color(x: Double, y: Double, color: Color, diametro: Int) {
         gr.setColor(color)
         gr.setPaint(color)
         gr.fillOval(scalex(x).toInt - diametro / 2, scaley(y).toInt - diametro / 2, diametro, diametro)
      }

      def bola_intense(x: Double, y: Double, intense: Float, diametro: Int) {
         gr.setColor(new Color(intense, intense, intense))
         gr.setPaint(new Color(intense, intense, intense))
         gr.fillOval(scalex(x).toInt - diametro / 2, scaley(y).toInt - diametro / 2, diametro, diametro)
      }

      def mostra() {
         top.getGraphics.drawImage(bi, 0, 0, null)
      }
   }

}