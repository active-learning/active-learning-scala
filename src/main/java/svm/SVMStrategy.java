/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package svm;

import java.io.BufferedReader;
import java.io.FileReader;

import svm.ml.active.BALANCED_EE;
import svm.ml.active.KFF;
import svm.ml.active.SELF_CONF;
import svm.ml.active.SIMPLE;
import weka.active.ActiveLearner;
import weka.classifiers.libsvm.SvmLib;
import weka.core.*;

public class SVMStrategy {

    ActiveLearner learner;
    DataContainer labeled = null;
    DataContainer unlabeled = null;
    private SvmLib svm = new SvmLib();

//    public SVMStrategy(String strategy, weka.core.Instances firstOfEachClass, Instances rest) throws Exception {
//    }

    /**
     * mutates both sets of instances!
     *
     * @param strategy
     * @param firstOfEachClass
     * @param rest
     * @throws Exception
     */
    public SVMStrategy(String strategy, Instances firstOfEachClass, Instances rest) throws Exception {
        unlabeled = svm.convertData(rest);
        labeled = svm.convertData(firstOfEachClass, unlabeled);
        switch (strategy) {
            case "SIMPLE":
                learner = new SIMPLE();
                break;
            case "SELF_CONF":
                learner = new SELF_CONF();
                break;
            case "KFF":
                learner = new KFF();
                break;
            case "BALANCED_EE":
                learner = new BALANCED_EE();
                break;
        }
    }

    public static Instances InstancesToInstances2(Instances insts) {
        Instances result = new Instances(insts, 0, 0);
        for (int i = 0; i < insts.numInstances(); i++) {
            Instance orig = insts.get(i);
            Instance2 inst2 = new Instance2(orig.weight(), orig.toDoubleArray());
            inst2.setDataset(result);
            result.add(inst2);
        }
        return result;
    }

    public static void main(String[] args) throws Exception {
        System.out.println("teste");
        BufferedReader dataReader = new BufferedReader(new FileReader("/home/davi/wcs/ucipp/uci/banana.arff"));
//        BufferedReader dataReader = new BufferedReader(new FileReader("/home/davi/wcs/ucipp/uci/iris.arff"));
        Instances data = InstancesToInstances2(new Instances(dataReader));
        dataReader.close();
        data.setClassIndex(data.numAttributes() - 1);

        Instances lab = new Instances(data, 0, 500);
        Instances unl = new Instances(data, 500, 500);

//        SVMStrategy s = new SVMStrategy("KFF", lab, unl);
//        SVMStrategy s = new SVMStrategy("SIMPLE", lab, unl);
        SVMStrategy s = new SVMStrategy("SELF_CONF", lab, unl);
//        SVMStrategy s = new SVMStrategy("BALANCED_EE", lab, unl);
        for (int i = 0; i < 100; i++) {
//            System.out.println(
            s.nextQuery();//);
        }
    }

    public Instance2 nextQuery() throws Exception {
        learner.buildClassifier(labeled);
        int ind = learner.instanceToQuery(unlabeled);
        InstanceContainer ins = unlabeled.getInstance(ind);
        unlabeled.deleteInstance(ind);
        System.out.println("idx " + ind + " class:" + ins.classValue());
//        System.out.println("");
        labeled.addInstance(ins);
        return null;
    }
}
