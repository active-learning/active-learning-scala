package weka.classifiers.libsvm;

public class svm_parameter implements java.io.Serializable {
    /* svm_type */
    public static final int C_SVC = 0;
    public int svm_type = C_SVC;
    public static final int NU_SVC = 1;
    public static final int ONE_CLASS = 2;
    public static final int EPSILON_SVR = 3;
    public static final int NU_SVR = 4;
    /* kernel_type */
    public static final int LINEAR = 0;
    public static final int POLY = 1;
    public static final int RBF = 2;
    public int kernel_type = RBF;
    public static final int SIGMOID = 3;
    public double degree = 3;    // for poly
    public double gamma = 1.0;    // for poly/rbf/sigmoid
    public double coef0 = 0.0;    // for poly/sigmoid

    // these are for training only
    public double cache_size = 40; // in MB
    public double eps = 1e-5;       // stopping criteria originally 1e-5
    public double C = 1000;           // for C_SVC, EPSILON_SVR and NU_SVR
    public double nu = 0.5;           // for NU_SVC, ONE_CLASS, and NU_SVR
    public double p = 0.1;         // for EPSILON_SVR
    public int shrinking = 1;      // use the shrinking heuristics
}
