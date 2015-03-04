package svmal.libsvm;

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
    public int kernel_type = LINEAR;
    public static final int SIGMOID = 3;
    public double degree = 3;    // for poly
    public double gamma = 1.0;    // for poly/rbf/sigmoid
    public double coef0 = 0.0;    // for poly/sigmoid

    // these are for training only
    public double cache_size = 200; // in MB
    public double eps = 0.1;       // stopping criteria originally 1e-5
    public double C = 1;           // for C_SVC, EPSILON_SVR and NU_SVR
    public double nu = 0.5;           // for NU_SVC, ONE_CLASS, and NU_SVR
    public double p = 0.1;         // for EPSILON_SVR
    public int shrinking = 0;      // use the shrinking heuristics
}
/*
 -S <int>
  Set type of SVM (default: 0)
    0 = C-SVC
    1 = nu-SVC
    2 = one-class SVM
    3 = epsilon-SVR
    4 = nu-SVR

 -K <int>
  Set type of kernel function (default: 2)
    0 = linear: u'*v
    1 = polynomial: (gamma*u'*v + coef0)^degree
    2 = radial basis function: exp(-gamma*|u-v|^2)
    3 = sigmoid: tanh(gamma*u'*v + coef0)

 -D <int>
  Set degree in kernel function (default: 3)

 -G <double>
  Set gamma in kernel function (default: 1/k)

 -R <double>
  Set coef0 in kernel function (default: 0)

 -C <double>
  Set the parameter C of C-SVC, epsilon-SVR, and nu-SVR
   (default: 1)

 -N <double>
  Set the parameter nu of nu-SVC, one-class SVM, and nu-SVR
   (default: 0.5)

 -Z
  Turns on normalization of input data (default: off)

 -J
  Turn off nominal to binary conversion.
  WARNING: use only if your data is all numeric!

 -V
  Turn off missing value replacement.
  WARNING: use only if your data has no missing values.

 -P <double>
  Set the epsilon in loss function of epsilon-SVR (default: 0.1)

 -M <double>
  Set cache memory size in MB (default: 40)

 -E <double>
  Set tolerance of termination criterion (default: 0.001)

 -H
  Turns the shrinking heuristics off (default: on)
 */