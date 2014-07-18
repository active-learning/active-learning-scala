package weka.classifiers.libsvm;

import java.io.Serializable;

public class svm_problem implements Serializable {
    public int l;
    public double[] y;
    public svm_node[][] x;
}
