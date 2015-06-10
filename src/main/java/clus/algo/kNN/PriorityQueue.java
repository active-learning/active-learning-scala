/*************************************************************************
 * Clus - Software for Predictive Clustering                             *
 * Copyright (C) 2007                                                    *
 *    Katholieke Universiteit Leuven, Leuven, Belgium                    *
 *    Jozef Stefan Institute, Ljubljana, Slovenia                        *
 *                                                                       *
 * This program is free software: you can redistribute it and/or modify  *
 * it under the terms of the GNU General Public License as published by  *
 * the Free Software Foundation, either version 3 of the License, or     *
 * (at your option) any later version.                                   *
 *                                                                       *
 * This program is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 * GNU General Public License for more details.                          *
 *                                                                       *
 * You should have received a copy of the GNU General Public License     *
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 *                                                                       *
 * Contact information: <http://www.cs.kuleuven.be/~dtai/clus/>.         *
 *************************************************************************/

package clus.algo.kNN;

import java.util.Vector;

/**
 * This class represents a queue with elements sorted by a double accompaning each element
 * When more elements are added then allowed, the element with the largest value is thrown away.
 * negative values are not allowed, they are used to indicate empty spaces.
 */
public class PriorityQueue {

	private Vector $elements;
	private double[] $values;

	public PriorityQueue(int size){
		$elements = new Vector(size,0);
		$values = new double[size];
		for (int i= 0;i<size;i++){
			$values[i]= -1;
		}
		$elements.setSize(getSize());
	}

	/**
	 * Returns the amount of objects possible in queue.
	 */
	public int getSize(){
		return $values.length;
	}

	/**
	 * Returns the index'th object in the queue.
	 * Require
	 *		index within range of queue : 0 < index < getSize()
	 */
	public Object getElement(int index){
		return $elements.elementAt(index);
	}
	/**
	 * Returns the value of the index'th object in the queue.
	 * Require
	 *		index within range of queue : 0 < index < getSize()
	 */
	public double getValue(int index){
		return $values[index];
	}

	/**
	 * Add given object to queue, place in queue will depend on its value.
	 * Require
	 *		no nullpointers : o != null
	 */
	public void addElement(Object o,double value){
		int i = 0;
		double curVal = getValue(i);
		while ((curVal>0)&&(curVal<= value)&&(i<getSize()-1)){
			i++;
			curVal = getValue(i);
		}
		if (i<getSize()){
			$elements.insertElementAt(o,i);
			$elements.setSize(getSize()); //makes last element fall off
			//have to shift some values to the right and add the new value
			for(int j=getSize()-1;j>i;j--){
				addValue(getValue(j-1),j);
			}
			addValue(value,i);
		}
	}

	// adds given value on given index in queue.
	private void addValue(double value,int index){
		$values[index] = value;
	}

	/**
	 * Prints the values of the queue to the standard output.
	 */
	public void printValues(){
		System.out.print("[");
		for (int i=0;i<getSize()-1;i++){
			System.out.print(getValue(i)+";");
		}
		System.out.println(getValue(getSize()-1)+"]");
	}
/*
	public static void main (String[] args){
		PriorityQueue q = new PriorityQueue(3);
		Integer a = new Integer(10);
		Integer b = new Integer(15);
		Integer c = new Integer(12);
		Integer d = new Integer(14);
		Integer e = new Integer(9);
		q.addElement(a,10);
		q.printValues();
		q.addElement(b,15);
		q.printValues();
		q.addElement(c,12);
		q.printValues();
		q.addElement(d,14);
		q.printValues();
		q.addElement(e,9);
		q.printValues();
		Integer x = (Integer) q.getElement(0);
		System.out.println(x.intValue());
	}
*/
}
