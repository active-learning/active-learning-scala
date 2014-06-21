/* This file is part of HS.
 * (C) Copyright 2008, Daniel Hsu */
#ifndef _SPATIAL_H
#define _SPATIAL_H

#include <cmath>
#include <ostream>
#include <vector>

template <typename T> inline T max(T x, T y) { return x >= y ? x : y; }
template <typename T> inline T min(T x, T y) { return x <= y ? x : y; }

const int NO_LABEL = -1;
const int NO_NODE = -1;

double rand_unif(void);

struct WeightedLeaf {
  int index;
  int label;
  double weight;
};

class Tree {
  public:
    int m; // number of nodes
    int n; // number of leaf nodes
    int nc; // number of classes { 0, 1, ..., nc-1 }

    // tree structure variables
    //
    int *parent; // parent indices
    int *lchild; // left child indices
    int *rchild; // right child indices
    int *size; // number of leaves in subtree
    int *depth; // maximum depth in subtree

    // bookkeeping variables
    //
    int **count; // node class label counts
    int *total; // total node class labels seen (total[i] = Sum_j count[i][j])
    double **ub; // upper bounds on true node class label counts
    double **lb; // lower bounds on true node class label counts
    bool **admissible; // flag indicating if (node,label) is admissible
    int *best_label; // best admissible label

    bool *split;
    double *cost;

    Tree(std::vector<int>& parent_vec, int num_classes);
    ~Tree();
    int select_node0(std::vector<int>& pruning);
    int select_node1(std::vector<int>& pruning);
    int sample_node(int node);
    void update(int leaf, int label, int subtree_root);
    void prune_node(int node, std::vector<int>& pruning);
    void report_all_leaves(int node, int label, std::ostream& os);
    double get_delta(double frac, int node) {
      double fs_corr = 1.0 - ((double) total[node]) / size[node];
      return fs_corr * 1.0 / total[node] + sqrt(fs_corr * frac * (1.0 - frac) / total[node]);
    }
};

#endif

/* EOF */
