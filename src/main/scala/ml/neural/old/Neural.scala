package ml.neural.old

import ml.Pattern

object Neural {

  import no.uib.cipr.matrix.DenseMatrix
  import no.uib.cipr.matrix.Matrices

//  sealed abstract class ActFun() {
//    def f(x: Double): Double
//  }
//
//  case class LinearOutputLayer() extends ActFun {
//    def f(x: Double) = x
//  }
//
//  case class Sigmoide() extends ActFun {
//    def f(x: Double) = (1.0 / (1 + math.exp(-x)) - 0.5) * 2d //better to avoid numerical instability //1.0 / (1 + math.exp(-x))
//  }
//
//  case class Tahn() extends ActFun {
//    def f(x: Double) = 1.7159 * math.tanh(2 * x / 3)
//  }
//
//  def patterns2matrices(insts: Seq[Pattern]) = {
//    val instsarray = insts.toArray
//    val transP = new DenseMatrix(instsarray.map(_.array))
//    val transT = new DenseMatrix(instsarray.map(x => x.weighted_label_array))
//    (transP, transT)
//  }

  /**
   * Moore-Penrose generalized inverse matrix.
   * Evita ill-conditioned matrices, i.e. singular ones (det = 0).
   * Based on the Java code, which was based on Huang Matlab code.
   * Theory:Ridge regression
   * MP(A) = inv((H'*H+lumda*I))*H'
   * @param H0
   * @return (M0, pseudo-inverse) M0 is used for incremental learning (OS-ELM)
   */
  def pinv(H0: DenseMatrix) = {
    val lumda = 0.0001
    val m = H0.numRows
    val n = H0.numColumns
    val H0T: DenseMatrix = new DenseMatrix(n, m)
    H0.transpose(H0T)
    val H0TH0: DenseMatrix = new DenseMatrix(n, n)
    H0T.mult(H0, H0TH0)
    val I: DenseMatrix = Matrices.identity(n)
    H0TH0.add(lumda, I)
    val H0TH0_inv: DenseMatrix = I.copy
    H0TH0.solve(I, H0TH0_inv)
    val pseudo_inverse: DenseMatrix = new DenseMatrix(n, m)
    H0TH0_inv.mult(H0T, pseudo_inverse)
    pseudo_inverse
  }

//  def inv(H0: DenseMatrix) = {
//    val I = Matrices.identity(H0.numColumns())
//    val Ainv = I.copy()
//    H0.solve(I, Ainv)
//    Ainv
//  }
//
//  def toArray(M: DenseMatrix) = ((0 until M.numRows) map {
//    r => ((0 until M.numColumns) map (c => M.get(r, c))).toArray
//  }).toArray
}