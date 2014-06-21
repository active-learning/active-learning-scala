/* This file is part of HS.
 * (C) Copyright 2008, Daniel Hsu */
#include <algorithm>
#include <cassert>
#include <cfloat>
#include <cstdio>
#include <cstdlib>
#include <ostream>
#include <stack>
#include <vector>

#include "spatial.h"

using std::endl;
using std::ostream;
using std::replace;
using std::stack;
using std::vector;

double rand_unif(void) {
  return (double) rand() / ((double) (RAND_MAX) + (double) 1.0);
}

Tree::Tree(vector<int>& parent_vec, int num_classes) {
  m = parent_vec.size();
  n = (m + 1) / 2;
  nc = num_classes;

  parent = new int[m];
  lchild = new int[m];
  rchild = new int[m];
  size = new int[m];
  depth = new int[m];

  count = new int*[m];
  total = new int[m];
  ub = new double*[m];
  lb = new double*[m];
  admissible = new bool*[m];
  best_label = new int[m];

  split = new bool[m];
  cost = new double[m];

  // initialize:
  // parent, {l,r}child, size,
  // counts, totals, admissible, best_labels, splits
  //
  for (int i = 0; i < m; i++) {
    parent[i] = NO_NODE;
    lchild[i] = NO_NODE;
    rchild[i] = NO_NODE;
    size[i] = 0;

    count[i] = new int[num_classes];
    total[i] = 0;
    ub[i] = new double[num_classes];
    lb[i] = new double[num_classes];
    admissible[i] = new bool[num_classes];
    best_label[i] = NO_LABEL;
    split[i] = false;

    for (int j = 0; j < num_classes; j++) {
      count[i][j] = 0;
      ub[i][j] = 1.0;
      lb[i][j] = 0.0;
      admissible[i][j] = false;
    }
  }

  // assign parent and {l,r}child pointers
  //
  for (int i = 0; i < m; i++) {
    int p = parent_vec[i];
    parent[i] = p;
    if (p == NO_NODE) continue;
    else if (lchild[p] == NO_NODE) lchild[p] = i;
    else rchild[p] = i;
  }

  // compute sizes and depths
  //
  for (int i = 0; i < m; i++) {
    if (lchild[i] != NO_NODE || rchild[i] != NO_NODE) continue;
    int p = i;
    int current_depth = 0;
    while ((p != NO_NODE) && (p < m) && (p >= 0)) {
      size[p]++;
      if (current_depth > depth[p]) depth[p] = current_depth;
      p = parent[p];
      current_depth++;
    }
  }

  // initialize costs
  //
  for (int i = 0; i < m; i++) {
    cost[i] = size[i];
  }
}

Tree::~Tree() {
  if (parent != NULL) delete [] parent;
  if (lchild != NULL) delete [] lchild;
  if (rchild != NULL) delete [] rchild;
  if (size != NULL) delete [] size;
  if (depth != NULL) delete [] depth;

  if (count != NULL) {
    for (int i = 0; i < m; i++)
      if (count[i] != NULL) delete [] count[i];
    delete [] count;
  }
  if (total != NULL) delete [] total;
  if (ub != NULL) {
    for (int i = 0; i < m; i++)
      if (ub[i] != NULL) delete [] ub[i];
    delete [] ub;
  }
  if (lb != NULL) {
    for (int i = 0; i < m; i++)
      if (lb[i] != NULL) delete [] lb[i];
    delete [] lb;
  }
  if (admissible != NULL) {
    for (int i = 0; i < m; i++)
      if (admissible[i] != NULL) delete [] admissible[i];
    delete [] admissible;
  }
  if (best_label != NULL) delete [] best_label;

  if (split != NULL) delete [] split;
  if (cost != NULL) delete [] cost;
}

// Choose node from 'pruning' with probability proportional to its number of
// unseen leaves and return its index.
//
int Tree::select_node0(vector<int>& pruning) {

  // "mass" of a node is its number of unseen leaves
  //
  vector<double> cumulative_mass;
  double total_mass = 0;
  for (size_t i = 0; i < pruning.size(); i++) {
    int idx = pruning[i];
    total_mass += size[idx] - total[idx];
    cumulative_mass.push_back(total_mass);
  }

  // pick a random number between 0 and the total mass
  //
  double val = rand_unif() * cumulative_mass.back();

  // determine the winner
  //
  for (size_t i = 0; i < cumulative_mass.size(); i++)
    if (cumulative_mass[i] >= val) return pruning[i];
  assert(val <= cumulative_mass.back());
  return NO_NODE;
}

// Choose node from 'pruning' with probability proportional to its bound and
// return its index.
//
int Tree::select_node1(vector<int>& pruning) {

  // "mass" of a node is its weighted error bound
  //
  vector<double> cumulative_mass;
  double total_mass = 0;
  for (size_t i = 0; i < pruning.size(); i++) {
    int idx = pruning[i];
    if (best_label[idx] == NO_LABEL) total_mass += size[idx];
    else total_mass += size[idx] - lb[idx][best_label[idx]];
    cumulative_mass.push_back(total_mass);
  }

  // pick a random number between 0 and the total mass
  //
  double val = rand_unif() * cumulative_mass.back();

  // determine the winner
  //
  for (size_t i = 0; i < cumulative_mass.size(); i++)
    if (cumulative_mass[i] >= val) return pruning[i];
  assert(val <= cumulative_mass.back());
  return NO_NODE;
}

// Choose a random unseen leaf under given node
//
int Tree::sample_node(int node) {
  int num_unseen_leaves = size[node] - total[node];

  if (num_unseen_leaves == 0) return NO_NODE;
  assert(num_unseen_leaves > 0);

  // base case
  //
  if (size[node] == 1) return node;

  // recursive step: choose child with probability proportional to its number
  // of unseen leaves and recurse.
  //
  assert(lchild[node] != NO_NODE && rchild[node] != NO_NODE);
  double p_left = ((double) size[lchild[node]] - total[lchild[node]])
    / num_unseen_leaves;
  if (rand_unif() < p_left) return sample_node(lchild[node]);
  else return sample_node(rchild[node]);
}

// Update data structure after having observed the label ('label') of the
// leaf ('leaf') sampled from a node ('subtree_root').
// This updates all nodes on the path from 'leaf' to 'subtree_root'.
//
void Tree::update(int leaf, int label, int subtree_root) {
  assert(leaf >= 0);
  assert(leaf < m);
  assert(size[leaf] == 1);
  assert(label >= 0 && label < nc);

  int i = leaf;
  while (i != NO_NODE) {

    // update empirical counts
    //
    count[i][label]++;
    total[i]++;

    // update bounds on true node class label counts
    //
    for (int l = 0; l < nc; l++) {
      double frac = ((double) count[i][l]) / total[i];
      double delta = get_delta(frac, i);
      double mean = frac * size[i];
      double err = delta * size[i];
      lb[i][l] = max((double) count[i][l], mean - err);
      ub[i][l] = min((double) size[i] - (total[i] - count[i][l]), mean + err);
    }

    // update admissibility flags and best admissible labels
    //
    double max_count = 0;
    for (int l = 0; l < nc; l++) {

      // (i,l) is admissible iff 1-lb[i][l] < 2*(1-ub[i][k]) for all k != l
      //
      admissible[i][l] = true;
      for (int k = 0; k < nc; k++)
        if (l != k && lb[i][l] <= 2.0 * ub[i][k] - size[i])
          admissible[i][l] = false;

      // choose the best admissible label
      //
      if (admissible[i][l] && count[i][l] > max_count) {
        max_count = count[i][l];
        best_label[i] = l;
      }
    }

    // update costs and splits
    //
    double basic_cost;
    if (best_label[i] != NO_LABEL)
      basic_cost = size[i] - lb[i][best_label[i]];
    else
      basic_cost = size[i];

    if (size[i] == 1)
      cost[i] = basic_cost;
    else {
      double split_cost = cost[lchild[i]] + cost[rchild[i]];
      if (split_cost < basic_cost && best_label[i] != NO_LABEL) {
        cost[i] = split_cost;
        split[i] = true;
      } else {
        cost[i] = basic_cost;
      }
    }

    // continue to parent unless at node
    //
    if (i != subtree_root) i = parent[i];
    else break;
  }
}

// Replace 'node' in 'pruning' with its best pruning
//
void Tree::prune_node(int node, vector<int>& pruning) {

  // replace 'node' with the last elt, then remove the last elt
  //
  replace(pruning.begin(), pruning.end(), node, pruning.back());
  pruning.pop_back();

  // add the best pruning of 'node'
  // (recursively descend on nodes that should be split, starting from 'node'
  // and stopping at any node that shouldn't be split)
  //
  stack<int> s;
  s.push(node);
  while (!s.empty()) {
    int i = s.top();
    s.pop();
    if (split[i]) {
      s.push(lchild[i]);
      s.push(rchild[i]);
    } else pruning.push_back(i);
  }
}

void Tree::report_all_leaves(int node, int label, ostream& os) {
  if (size[node] == 1) {
    os << node << " " << label << endl;
  } else {
    if (lchild[node] != NO_NODE) {
      report_all_leaves(lchild[node], label, os);
    }
    if (rchild[node] != NO_NODE) {
      report_all_leaves(rchild[node], label, os);
    }
  }
}

/* EOF */
