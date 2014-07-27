/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package svmal;

import ml.Pattern;
import svmal.libsvm.SvmLib;
import svmal.ml.active.BALANCED_EE;
import svmal.ml.active.KFF;
import svmal.ml.active.SELF_CONF;
import svmal.ml.active.SIMPLE;
import svmal.weka.active.ActiveLearner;
import svmal.weka.core.DataContainer;
import svmal.weka.core.Instance2;
import svmal.weka.core.InstanceContainer;
import weka.core.Instance;
import weka.core.Instances;

import java.io.BufferedReader;
import java.io.FileReader;

public class SVMStrategymulti extends SVMStrategy {

    public SVMStrategymulti(String strategy, Instances firstOfEachClass, Instances rest) throws Exception {
        super(strategy, firstOfEachClass, rest);
    }

    public static Instances PatternsToInstances2(Pattern[] patts, int positiveLabel) {
        Instances result = new Instances(patts[0].dataset(), 0, 0);
        int classIndex = result.classIndex();
        for (Pattern orig : patts) {
            double[] vals = orig.toDoubleArray();
            Instance2 inst2;
            if (vals[classIndex] == positiveLabel) vals[classIndex] = 1;
            else vals[classIndex] = 0;
            inst2 = new Instance2(orig.weight(), vals);
            inst2.setIndex(orig.id());
            inst2.setDataset(result);
            result.add(inst2);
        }
        return result;
    }
}
