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

import clus.main.*;
import clus.model.ClusModel;
import clus.data.rows.*;
import clus.statistic.ClusStatistic;
import clus.statistic.StatisticPrintInfo;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

import jeans.util.MyArray;

public class KNNModel implements ClusModel {

	public ClusStatManager m_SMgr;

	private RowData $trainingData;

	private VectorDistance $vectDist; //used for calculating distances

	public KNNModel(ClusRun clRun,VectorDistance vd){
		$trainingData = (RowData) clRun.getTrainingSet();
		m_SMgr = clRun.getStatManager();

		$vectDist = vd;

	}

	public String getModelInfo() {
		return "KNNModel";
	}

	/**
	 * This method predicts the value for the given tuple and returns
	 * some statistic.
	 */
	public ClusStatistic predictWeighted(DataTuple tuple){
		ClusStatistic stat = m_SMgr.createClusteringStat();
		//find out how much neighbours necessary (via settings)
		int amountNBS = Settings.kNN_k.getValue();
		//find out if Distance-Weighted kNN used
		boolean distWeighted = Settings.kNN_distWeighted.getValue();

		//make a priorityqueue of size amountNBS to find nearest neighbours
		PriorityQueue q = new PriorityQueue(amountNBS);

		int nbr = $trainingData.getNbRows();
		DataTuple curTup;
		double dist;
		//find the nearest neighbours
		for (int i = 0; i <nbr;i++){
			curTup = $trainingData.getTuple(i);
			dist = calcDistance(tuple,curTup);
			q.addElement(curTup,dist);
		}
		//add the kNN's to the statistic

		//weights all the same for now
		double weight = 1.0;
		for (int i=0;i<amountNBS;i++){
			//Change weights when distance-weighted kNN is wanted
			if (distWeighted) weight = 1.0 / Math.pow(q.getValue(i),2);
			stat.updateWeighted((DataTuple)q.getElement(i),weight);
		}
		stat.calcMean();
		return stat;
	}

	// Calculates distance between 2 tuples
	private double calcDistance(DataTuple t1,DataTuple t2){
		return $vectDist.getDistance(t1,t2);
	}


	//?
	public void applyModelProcessors(DataTuple tuple, MyArray mproc) throws IOException{
	}

	//?
	public int getModelSize(){return 0;}

	public void printModel(PrintWriter wrt) {
	}

	public void printModelAndExamples(PrintWriter wrt, StatisticPrintInfo info, RowData examples) {
	}

	public void printModel(PrintWriter wrt, StatisticPrintInfo info) {
	}

	public void printModelToPythonScript(PrintWriter wrt) {
	}

	public void printModelToQuery(PrintWriter wrt, ClusRun cr, int starttree, int startitem, boolean ex) {
	}
	public void attachModel(HashMap table) {
		System.err.println(getClass().getName()+"attachModel() not implemented");
	}

/*
	public final ClusStatistic getTotalStat() {
		return m_TotStat;
	}

	public final double getTotWeight() {
		return m_TotStat.m_SumWeight;
	}

	public final void setTotalStat(ClusStatistic stat) {
		m_TotStat = stat;
	}

	public final void initTotalStat(ClusStatManager smgr) {
		m_TotStat = smgr.createStatistic();
	}*/

  public int getID() {
  	return 0;
  }

  public ClusModel prune(int prunetype) {
	return this;
  }

  public void retrieveStatistics(ArrayList list) {
  }
}
