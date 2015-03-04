/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package svmalk;

import ml.Pattern;
import svmalk.weka.core.Instance2;
import weka.core.Instances;

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
