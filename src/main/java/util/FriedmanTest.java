package util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.text.DecimalFormat;
import java.util.*;

import weka.core.Utils;

/**
 * Executa o teste de Friedman e de Nemenyi
 *
 * @author thiago
 */
public class FriedmanTest {

    static final double[] qAlpha1pct = {2.576, 2.913, 3.113, 3.255, 3.364, 3.452, 3.526, 3.590, 3.646, 3.696, 3.741, 3.781, 3.818, 3.853, 3.884, 3.914,
            3.941, 3.967, 3.992, 4.015, 4.037, 4.057, 4.077, 4.096, 4.114, 4.132, 4.148, 4.164, 4.179, 4.194, 4.208, 4.222, 4.236, 4.249, 4.261, 4.273, 4.285,
            4.296, 4.307, 4.318, 4.329, 4.339, 4.349, 4.359, 4.368, 4.378, 4.387, 4.395, 4.404};
    static final double[] qAlpha5pct = {1.960, 2.344, 2.569, 2.728, 2.850, 2.948, 3.031, 3.102, 3.164, 3.219, 3.268, 3.313, 3.354, 3.391,
            3.426, 3.458, 3.489, 3.517, 3.544, 3.569, 3.593, 3.616, 3.637, 3.658, 3.678, 3.696, 3.714, 3.732};
    static final double[] qAlpha10pct = {1.645, 2.052, 2.291, 2.460, 2.589, 2.693, 2.780, 2.855, 2.920, 2.978, 3.030, 3.077, 3.120, 3.159,
            3.196, 3.230, 3.261, 3.291, 3.319, 3.346, 3.371, 3.394, 3.417, 3.439, 3.459, 3.479, 3.498, 3.516};

    static final double[] qAlphaMaior = qAlpha5pct;
    static final double[] qAlphaMenor = qAlpha1pct;

    public static int[][] Friedman(double[][] matriz, boolean asc) {
        StringBuilder statsResults = new StringBuilder();
        int nl = matriz.length;
        try {
            int nc = matriz[0].length;
        } catch (ArrayIndexOutOfBoundsException ex) {
            System.out.println("no algorithms provided");
            System.exit(0);
        }
        int nc = matriz[0].length;
        int[][] table = new int[nc][nc]; //invertido (indica qnd lin vence col; 1:0.9; 2:0.95)
        double[][] ranks = new double[nl][nc];
        double F_Friedman = Double.NaN;
        double chiFriedman = Double.NaN;
        try {
            for (int i = 0; i < nl; ++i) {
                int[] sorts = Utils.sort(matriz[i]);
                for (int j = 0; j < nc; ++j) {
                    if (!asc) {
                        ranks[i][sorts[j]] = j + 1;
                    } else {
                        ranks[i][sorts[j]] = nc - j;
                    }
                }
                BitSet modificados = new BitSet(nc);
                modificados.clear();
                for (int j = 0; j < nc; ++j) {
                    if (modificados.get(j)) {
                        continue;
                    }
                    double val = matriz[i][j];
                    int iguais = 1;
                    double soma = ranks[i][j];

                    for (int jj = j + 1; jj < nc; ++jj) {
                        if (matriz[i][jj] == val) {
                            ++iguais;
                            soma += ranks[i][jj];
                        }
                    }
                    if (iguais > 1) {
                        for (int jj = j; jj < nc; ++jj) {
                            if (matriz[i][jj] == val) {
                                modificados.set(jj);
                                ranks[i][jj] = soma / ((double) iguais);
                            }
                        }
                    }

                }
            }

            double[] medRank = new double[nc];
            for (int j = 0; j < nc; ++j) {
                medRank[j] = 0;
                for (int i = 0; i < nl; ++i) {
                    medRank[j] += ranks[i][j];
                }
                medRank[j] /= nl;
            }
            int[] orderedRank = Utils.sort(medRank);
//            System.out.println(Arrays.toString(medRank));
//            System.out.println(Arrays.toString(orderedRank));
            statsResults.append("Ranking:\n");
            for (int j = 0; j < nc; ++j) {
                statsResults.append((orderedRank[j] + 1) + "\t" + medRank[orderedRank[j]] + "\n");
            }

            chiFriedman = (12.0 * nl) / (nc * (nc + 1));
            double tmp = 0;
            for (int j = 0; j < nc; ++j) {

                tmp += medRank[j] * medRank[j];
            }
            chiFriedman *= (tmp - (nc * (((nc + 1) * (nc + 1)) / 4.0)));
            F_Friedman = ((nl - 1) * chiFriedman) / ((nl * (nc - 1)) - chiFriedman);
            statsResults.append("\nChiFriedman: " + chiFriedman + " F_Friedman: " + F_Friedman + " df1: " + (nc - 1) + " df2:" + (nc - 1) * (nl - 1) + "\n");
            double pValue_Friedman = weka.core.Statistics.FProbability(F_Friedman, nc - 1, (nc - 1) * (nl - 1));


            /**
             * daqui para baixo tem a ver com o nemenyi
             */
            double critDiff = Math.sqrt((nc * (nc + 1.0)) / (6.0 * nl));

            System.out.println("Friedman teste pvalue: " + pValue_Friedman);
//            statsResults.append("Friedman teste pvalue: ").append(pValue_Friedman).append("\n\n");
            statsResults.append("Nemenyi Test:");
            int[] sorts = Utils.sort(medRank);
            statsResults.append("CritDiff(.01) ").append(qAlphaMenor[nc - 2] * critDiff).append("\n");
//            statsResults.append("CritDiff(.10) ").append(qAlpha10pct[nc - 2] * critDiff).append("\n");
            for (int j = 0; j < nc; ++j) {
                for (int jj = j + 1; jj < nc; ++jj) {
                    //statsResults.append("C"+sorts[nc-j-1]+"/C"+sorts[nc-jj-1]+" "+ (medRank[sorts[nc-j-1]]-medRank[sorts[nc-jj-1]])+"\n");
                    double diff = medRank[sorts[nc - j - 1]] - medRank[sorts[nc - jj - 1]];
                    if (diff >= (qAlphaMenor[nc - 2] * critDiff)) {
                        statsResults.append(sorts[nc - j - 1] + 1).append(" ").append(sorts[nc - jj - 1] + 1).append(" ").append(medRank[sorts[nc - j - 1]] - medRank[sorts[nc - jj - 1]]).append(" " + medRank[sorts[nc - j - 1]] + " " + medRank[sorts[nc - jj - 1]]).append("\n");
                    }
                }
            }


            //monta "tabela" de comparação par a par
            statsResults.append("\n\nTabela de Comparação (+ significa diferença estatística 90% e * 95%)\n");
            statsResults.append("A marcação em uma célula indica que o algoritmo da linha foi pior que o da coluna\n");
            statsResults.append("Os números nas linhas e colunas são referentes as colunas do arquivo de entrada " +
                    "lembrando que cada coluna é considerada como um algoritmo\n\n");

            statsResults.append(",,");
            for (int j = 1; j <= nc; ++j) {
                statsResults.append(j).append(",");
            }
            /**
             * algoritmo da coluna j melhor que da linha i com x% de confianca
             */
            for (int i = 0; i < nc; i++) {
                statsResults.append("\n,").append(i + 1).append(",");
                for (int j = 0; j < nc; j++) {
                    double diff = medRank[i] - medRank[j];
                    if (diff >= (qAlphaMaior[nc - 2] * critDiff) && diff < (qAlphaMenor[nc - 2] * critDiff)) {
                        table[j][i] = 1;
                    } else if (diff >= (qAlphaMenor[nc - 2] * critDiff)) {
                        table[j][i] = 2;
                    }
                }
            }

        } catch (Exception e) {
            System.out.println("|||erro calculando pvalue Friedman: " + "nc=" + nc + "--nl=" + nl + "--XF=" + chiFriedman + "--FF=" + F_Friedman + "\n" + e.getMessage());
            e.printStackTrace();

        }

//        System.out.println(statsResults);

        return table;
    }

    /**
     * List is the ranking of avgs.
     */
    public static LinkedList<Number> CD(double[][] matriz, boolean asc) {
        LinkedList<Number> l = new LinkedList<Number>();

        int nl = matriz.length;
        int nc = matriz[0].length;
        double[][] ranks = new double[nl][nc];
        double F_Friedman = Double.NaN;
        double chiFriedman = Double.NaN;
        try {
            for (int i = 0; i < nl; ++i) {
                int[] sorts = Utils.sort(matriz[i]);
                for (int j = 0; j < nc; ++j) {
                    if (!asc) {
                        ranks[i][sorts[j]] = j + 1;
                    } else {
                        ranks[i][sorts[j]] = nc - j;
                    }
                }
                BitSet modificados = new BitSet(nc);
                modificados.clear();
                for (int j = 0; j < nc; ++j) {
                    if (modificados.get(j)) {
                        continue;
                    }
                    double val = matriz[i][j];
                    int iguais = 1;
                    double soma = ranks[i][j];

                    for (int jj = j + 1; jj < nc; ++jj) {
                        if (matriz[i][jj] == val) {
                            ++iguais;
                            soma += ranks[i][jj];
                        }
                    }
                    if (iguais > 1) {
                        for (int jj = j; jj < nc; ++jj) {
                            if (matriz[i][jj] == val) {
                                modificados.set(jj);
                                ranks[i][jj] = soma / ((double) iguais);
                            }
                        }
                    }

                }
            }

            double[] medRank = new double[nc];
            for (int j = 0; j < nc; ++j) {
                medRank[j] = 0;
                for (int i = 0; i < nl; ++i) {
                    medRank[j] += ranks[i][j];
                }
                medRank[j] /= nl;
            }
            int[] orderedRank = Utils.sort(medRank);

            chiFriedman = (12.0 * nl) / (nc * (nc + 1));
            double tmp = 0;
            for (int j = 0; j < nc; ++j) {
                tmp += medRank[j] * medRank[j];
            }
            chiFriedman *= (tmp - (nc * (((nc + 1) * (nc + 1)) / 4.0)));
            F_Friedman = ((nl - 1) * chiFriedman) / ((nl * (nc - 1)) - chiFriedman);
            double critDiff = qAlpha10pct[nc - 2] * Math.sqrt((nc * (nc + 1.0)) / (6.0 * nl));
            double limit = medRank[orderedRank[0]] + critDiff;
            int j = 0;
            while (j < nc && medRank[orderedRank[j]] <= limit) {
                l.add(orderedRank[j]);
                j++;
            }

        } catch (Exception e) {
            System.out.println("|||erro calculando pvalue Friedman: " + "nc=" + nc + "--nl=" + nl + "--XF=" + chiFriedman + "--FF=" + F_Friedman + "\n" + e.getMessage());
            e.printStackTrace();
        }
        return l;
    }

//    /**
//     * Monta uma tabela de Vitorias/Empates/Derrotas
//     *
//     * @param data dados, algoritmos em colunas, bases nas linhas
//     * @param asc  ascendente ou descendente
//     * @return
//     */
//    public static String tabelaDesempenho(double[][] data, boolean asc) {
//        StringBuilder output = new StringBuilder();
//        int numAlgoritmos = data[0].length;
//        int numBases = data.length;
//        int[][] vitorias = new int[numAlgoritmos][numAlgoritmos];
//        int[][] empates = new int[numAlgoritmos][numAlgoritmos];
//        int[][] derrotas = new int[numAlgoritmos][numAlgoritmos];
//
//        for (int i = 0; i < numAlgoritmos; ++i) {
//
//            for (int j = 0; j < numAlgoritmos; ++j) {
//                if (i == j) {
//                    continue;
//                }
//
//                for (int b = 0; b < numBases; ++b) {
//                    if (data[b][i] > data[b][j]) {
//                        if (asc) {
//                            ++vitorias[i][j];
//                        } else {
//                            ++derrotas[i][j];
//                        }
//                    } else if (data[b][i] < data[b][j]) {
//                        if (asc) {
//                            ++derrotas[i][j];
//                        } else {
//                            ++vitorias[i][j];
//                        }
//                    } else {
//                        ++empates[i][j];
//                    }
//                }
//            }
//        }
//        double pVit, pEmp, pDer;
//        Locale.setDefault(Locale.US);
//        DecimalFormat df = new DecimalFormat("0.0");
//        for (int i = 0; i < numAlgoritmos; ++i) {
//            output.append("\n");
//            for (int j = 0; j < numAlgoritmos; ++j) {
//                if (i == j) {
//                    output.append("---,");
//                } else {
//                    pVit = (vitorias[i][j] / (1.0 * numBases)) * 100;
//                    pEmp = (empates[i][j] / (1.0 * numBases)) * 100;
//                    pDer = (derrotas[i][j] / (1.0 * numBases)) * 100;
//                    output.append(df.format(pVit)).append(" / ").append(df.format(pEmp)).append(" / ").append(df.format(pDer)).append(",");
//                }
//            }
//        }
//        return output.toString();
//    }
//
//    /**
//     * Monta uma tabela de Contrast Estimation (Garcia2010)
//     *
//     * @param data dados, algoritmos em colunas, bases nas linhas
//     * @param asc  ascendente ou descendente
//     * @return
//     */
//    public static String tabelaConstrastEstimation(double[][] data, boolean asc) {
//        StringBuilder output = new StringBuilder();
//        int numAlgoritmos = data[0].length;
//        int numBases = data.length;
//        double[][] Zs = new double[numAlgoritmos][numAlgoritmos];
//        double[] Ms = new double[numAlgoritmos];
//        int middle = numBases / 2;
//
//        for (int i = 0; i < numAlgoritmos - 1; ++i) {
//
//            for (int j = i + 1; j < numAlgoritmos; ++j) {
//                double[] diferencas = new double[numBases];
//                for (int b = 0; b < numBases; ++b) {
//                    diferencas[b] = data[b][i] - data[b][j];
//                }
//                Arrays.sort(diferencas);
//                if (numBases % 2 == 0) {
//                    Zs[i][j] = Zs[j][i] = (diferencas[middle] + diferencas[middle - 1]) / 2.0;
//                } else {
////                    System.out.println("" + i + "|" + j + ":" + diferencas[middle]);
//                    Zs[i][j] = Zs[j][i] = diferencas[middle];
//                }
//            }
//
//        }
//
//        for (int i = 0; i < numAlgoritmos; ++i) {
//            double sum = 0;
//            for (int j = 0; j < numAlgoritmos; ++j) {
//                sum += Zs[i][j];
//            }
//            Ms[i] = sum / numAlgoritmos;
//        }
//
////        for (int i = 0; i < numAlgoritmos; ++i) {
////            for (int j = 0; j < numAlgoritmos; ++j) {
////                System.out.print(Utils.doubleToString(Ms[i] - Ms[j], 4) + ";");
////            }
////            System.out.println();
////        }
//        return output.toString();
//    }

    public static void main(String[] args) {
        String arq = "/home/davi/unversioned/organizar/friedman/elmc.txt";
        try {
//            //-f /home/thiago/Backup_experimentos/ESMEM_prms/tabela_ems.csv -t
//            if (args.length == 0) {
//                Scanner input = new Scanner(System.in);
//                System.out.println("Digite os parametros (-h para ajuda)");
//                args = input.nextLine().split(" ");
//            }
//            arq = Utils.getOption("f", args);
//            boolean help = Utils.getFlag("h", args);
//            if (help) {
//                System.out.println("Forma de uso: java -jar FriedmanTest.jar -f arquivo -t" +
//                                "\nO arquivo deve estar formatado da seguinte forma:\n" +
//                                "1ª linha: NUM_LINHAS TAB NUM_COLUNAS TAB ASC ou DESC\n" +
//                                "\tUse ASC quando a medida sendo analisada é da forma quanto maior melhor e " +
//                                "DESC caso contrário. \n\tOs algoritmos são considerados como as colunas e as bases de dados" +
//                                "as linhas\nNas linhas seguintes coloque os valores separados por TAB e usando . (ponto) como " +
//                                "separador decimal\nCaso necessário use # para comentar linhas fazendo com que o script as ignore" +
//                                "\n\nA opção -n realiza o teste de Friedman\n\n" +
//                                "\n\nA opção -t faz com que seja apresentada a tabela de vitórias/empates/derrotas." +
//                                "\n\nA opção -c faz com que também seja apresentada a tabela de Contrast Estiamtion."
//                );
//                return;
//            }
            boolean tabelaVED = true; //Utils.getFlag("t", args);
            boolean friedman = true; //Utils.getFlag("n", args);
            boolean contrastEstimation = true; //Utils.getFlag("c", args);

            double[][] data;
            BufferedReader reader = new BufferedReader(new FileReader(new File(arq)));
            String line = reader.readLine();

            int lin = 0, col = 0;
            //System.out.println(line);
            StringTokenizer st = new StringTokenizer(line, "\t");
            data = new double[Integer.parseInt(st.nextToken())][Integer.parseInt(st.nextToken())];
            boolean asc = st.nextToken().equalsIgnoreCase("asc");
            while ((line = reader.readLine()) != null) {
                //System.out.println(line);
                //pula comentarios
                if (line.startsWith("#")) {
                    continue;
                }
                st = new StringTokenizer(line, "\t");
                while (st.hasMoreElements()) {
                    String token = st.nextToken();
                    //System.out.println(token);
                    data[lin][col] = Float.parseFloat(token);
                    col++;
                }
                lin++;
                //para se leu tudo, assim no final do arquivo podem ter comentarios
                if (lin == data.length) {
                    break;
                }
                col = 0;
            }
            String ascORdesc = (asc) ? "ASC" : "DESC";
            if (friedman) {
//                System.out.println("\n\nEstatísticas -- " + arq + "(" + ascORdesc + ")\n" + FriedmanTest.Friedman(data, asc));
            }
            if (tabelaVED) {
//                System.out.println("\n\nWin/Tie/Loss -- " + arq + "\n" + FriedmanTest.tabelaDesempenho(data, asc));
            }
            if (contrastEstimation) {
//                System.out.println("\n\nConstrast Estimation based on medians -- " + arq + "\n" + FriedmanTest.tabelaConstrastEstimation(data, asc));
            }
        } catch (Exception e) {
            System.out.println("ARQUIVO: " + arq);
            e.printStackTrace();
        }
    }
}
