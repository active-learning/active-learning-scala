package weka.classifiers.libsvm;

//
// svm_model
//
public class svm_model implements java.io.Serializable {
    public svm_parameter param;    // parameter
    public int nr_class;        // number of classes, = 2 in regression/one class svm
    public int l;            // total #SV
    public svm_node[][] SV;    // SVs (SV[l])
    public double[][] sv_coef;    // coefficients for SVs in decision functions (sv_coef[n-1][l])
    public double[] rho;        // constants in decision functions (rho[n*(n-1)/2])

    // for classification only

    public int[] label;        // label of each class (label[n])
    public int[] nSV;        // number of SVs for each class (nSV[n])
    // nSV[0] + nSV[1] + ... + nSV[n-1] = l
}
