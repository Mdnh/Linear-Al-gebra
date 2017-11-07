package algebra;

public class Algebra {

    static double abs(double a) {
        if (a > 0) {
            return a;
        }
        return -a;
    }

    static double[] gauss(double[][] A) {
        int n = A.length;

        for (int i = 0; i < n; i++) {
            // Search for maximum in this column
            double maxEl = abs(A[i][i]);
            int maxRow = i;
            for (int k = i + 1; k < n; k++) {
                if (abs(A[k][i]) > maxEl) {
                    maxEl = abs(A[k][i]);
                    maxRow = k;
                }
            }

            // Swap maximum row with current row (column by column)
            for (int k = i; k < n + 1; k++) {
                double tmp = A[maxRow][k];
                A[maxRow][k] = A[i][k];
                A[i][k] = tmp;
            }

            // Make all rows below this one 0 in current column
            for (int k = i + 1; k < n; k++) {
                double c = -A[k][i] / A[i][i];
                for (int j = i; j < n + 1; j++) {
                    if (i == j) {
                        A[k][j] = 0;
                    } else {
                        A[k][j] += c * A[i][j];
                    }
                }
            }
        }

        // Solve equation Ax=b for an upper triangular matrix A
        double[] x = new double[n];
        for (int i = n - 1; i >= 0; i--) {
            x[i] = A[i][n] / A[i][i];
            for (int k = i - 1; k >= 0; k--) {
                A[k][n] -= A[k][i] * x[i];
            }
        }
        return x;
    }

    static double det(int n, double mat[][]) {
        double d = 0;
        int c, subi, i, j, subj;
        double submat[][] = new double[10][10];
        if (n == 2) {
            return ((mat[0][0] * mat[1][1]) - (mat[1][0] * mat[0][1]));
        } else {
            for (c = 0; c < n; c++) {
                subi = 0;
                for (i = 1; i < n; i++) {
                    subj = 0;
                    for (j = 0; j < n; j++) {
                        if (j == c) {
                            continue;
                        }
                        submat[subi][subj] = mat[i][j];
                        subj++;
                    }
                    subi++;
                }
                d = d + (Math.pow(-1, c) * mat[0][c] * det(n - 1, submat));
            }
        }
        return d;
    }

    public static void main(String[] args) {
        new Frame();
    }

    public static double[][] power(int n, double a[][]) {
        double b[][] = new double[10][10];
        double c[][] = new double[10][10];
        int r = a.length;
        int i, j, k, P, u, t;
        for (P = 1; P <= n; P++) {
            if (P == 1) {
                for (u = 0; u < r; u++) {
                    for (t = 0; t < r; t++) {
                        if (t == u) {
                            c[t][u] = 1;
                        } else {
                            c[t][u] = 0;
                        }
                    }
                }
            }
            for (i = 0; i < r; i++) {
                for (j = 0; j < r; j++) {
                    b[i][j] = 0;
                    for (k = 0; k < r; k++) {
                        b[i][j] = b[i][j] + a[i][k] * c[k][j];
                    }
                }
            }
            for (i = 0; i < r; i++) {
                for (j = 0; j < r; j++) {
                    c[i][j] = b[i][j];
                }
            }
        }
        return c;
    }

    public static double[][] multiply(double[][] first, double[][] second) {
        int m = first.length;
        int n = first[0].length;
        int p = second.length;
        int q = second[0].length;
        double sum = 0;
        double[][] multiply = new double[m][q];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < q; j++) {
                for (int k = 0; k < p; k++) {
                    sum = sum + first[i][k] * second[k][j];
                }
                multiply[i][j] = sum;
                sum = 0;
            }
        }
        return multiply;
    }

    static public double[][] inversed(double A[][]) {
        int n = A.length;
        double [][]h = new double [n][n];
        

        for (int i = 0; i < n; i++) {
            // Search for maximum in this column
            double maxEl = abs(A[i][i]);
            int maxRow = i;
            for (int k = i + 1; k < n; k++) {
                if (abs(A[k][i]) > maxEl) {
                    maxEl = abs(A[k][i]);
                    maxRow = k;
                }
            }

            // Swap maximum row with current row (column by column)
            for (int k = i; k < n; k++) {
                double tmp = A[maxRow][k];
                A[maxRow][k] = A[i][k];
                A[i][k] = tmp;
                
                double tmp2 = h[maxRow][k];
                h[maxRow][k] = h[i][k] ;
                h[i][k] = tmp;
                
            }

            // Make all rows below this one 0 in current column
            for (int k = i + 1; k < n; k++) {
                double c = -A[k][i] / A[i][i];
                for (int j = i; j < n; j++) {
                    if (i == j) {
                        A[k][j] = 0;
                        h[k][i] = 0 ;
                    } else {
                        A[k][j] += c * A[i][j];
                        h[k][i]+=c * h[i][j];
                    }
                }
            }
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (A[i][i] == 0) {
                        continue;
                    }
                    A[j][k] /= A[i][i];
                    if(h[j][k]!=0)
                        h[j][k]/=h[i][i];
                    
                }
            }
        }
        return (h);
    }

    static double[][] grade(double[][] A) {
        int n = A.length;

        for (int i = 0; i < n; i++) {
            // Search for maximum in this column
            double maxEl = abs(A[i][i]);
            int maxRow = i;
            for (int k = i + 1; k < n; k++) {
                if (abs(A[k][i]) > maxEl) {
                    maxEl = abs(A[k][i]);
                    maxRow = k;
                }
            }

            // Swap maximum row with current row (column by column)
            for (int k = i; k < n; k++) {
                double tmp = A[maxRow][k];
                A[maxRow][k] = A[i][k];
                A[i][k] = tmp;
            }

            // Make all rows below this one 0 in current column
            for (int k = i + 1; k < n; k++) {
                double c = -A[k][i] / A[i][i];
                for (int j = i; j < n; j++) {
                    if (i == j) {
                        A[k][j] = 0;
                    } else {
                        A[k][j] += c * A[i][j];
                    }
                }
            }
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (A[i][i] == 0) {
                        continue;
                    }
                    A[j][k] /= A[i][i];
                }
            }
        }
        return (A);
    }
}
