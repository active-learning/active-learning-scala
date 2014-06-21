/* This file is part of HS.
 * (C) Copyright 2008, Daniel Hsu */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include <getopt.h>

#include "spatial.h"

#define die(fmt, ...) do { fprintf(stderr, fmt " [%s:%d]\n", ##__VA_ARGS__, __FILE__, __LINE__); exit(-1); } while (0)

using std::ifstream;
using std::istringstream;
using std::ofstream;
using std::string;
using std::vector;

const int SELECT_RANDOM = 0;
const int SELECT_ACTIVE = 1;

void load_col_vector(char *filename, vector<int>& v) {
  string tmp_str;
  ifstream file;

  file.open(filename, ifstream::in);
  if (file.fail())
    die("load_col_vector(): error opening file %s", filename);

  while (getline(file, tmp_str)) {
    istringstream is(tmp_str);
    int elt;
    is >> elt;
    v.push_back(elt);
  }

  file.close();
}

vector<int> *load_labels(char *filename) {
  vector<int> *labels = new vector<int>;
  load_col_vector(filename, *labels);
  return labels;
}

Tree *load_tree(char *filename, int num_classes) {
  vector<int> parent;
  load_col_vector(filename, parent);
  return new Tree(parent, num_classes);
}

void test_sampler(Tree &tree, Tree &opt, vector<int>& labels, int sel_type, int period, char *output_prefix) {

  for (int i = 0; i < opt.n; i++)
    opt.update(i, labels[i], opt.m - 1);

  vector<int> pruning;
  pruning.push_back(tree.m - 1);
  for (int i = 0; i < tree.n; i++) {

    // select a node from the current pruning to sample
    int node;
    node = tree.select_node1(pruning);

    // sample a leaf from the subtree rooted at selected node,
    // and obtain its label
    int leaf = tree.sample_node(node);
    printf("%d\n", leaf);
    int leaf_label = labels[leaf];

    // update tree and pruning
    tree.update(leaf, leaf_label, node);
    tree.prune_node(node, pruning);












/*
    // do accounting
    //
    int num_mistakes = opt.n;
    double mistake_bound = opt.n;
    for (vector<int>::const_iterator n = pruning.begin(); n != pruning.end(); n++) {
      int l;
      if (tree.best_label[*n] != NO_LABEL)
        l = tree.best_label[*n];
      else if (tree.parent[*n] != NO_NODE) {
        l = tree.best_label[tree.parent[*n]];
        assert(l != NO_LABEL);
      } else {
        l = 0; // no admissible label for root, so use '0' as a default
      }
      num_mistakes -= opt.count[*n][l];
      mistake_bound -= tree.lb[*n][l];
    }
*/
    // report statistics
    // (number of label queries, error, error bound, pruning size)
    //
/*
    printf("%d %f %f %d\n", i+1,
        ((double) num_mistakes) / tree.n,
        mistake_bound / tree.n,
        pruning.size());
*/
    // periodically report the leaves and their assigned labels
    //
/*
    if (((i+1) % period == 0) || i == tree.n - 1) {
      ofstream output_file;

      // generate filename
      //
      char output_suffix[10];
      snprintf(output_suffix, 10, ".%d", i+1);
      string filename(output_prefix);
      string suffix_str(output_suffix);
      filename.append(suffix_str);

      // write to file
      //
      output_file.open(filename.c_str());
      if (output_file.fail())
        die("test_sampler(): error opening %s", filename.c_str());

      for (vector<int>::const_iterator n = pruning.begin(); n != pruning.end(); n++) {
        int l;
        if (tree.best_label[*n] != NO_LABEL)
          l = tree.best_label[*n];
        else if (tree.parent[*n] != NO_NODE) {
          l = tree.best_label[tree.parent[*n]];
          assert(l != NO_LABEL);
        } else {
          l = 0; // no admissible label for root, so use '0' as a default
        }
        tree.report_all_leaves(*n, l, output_file);
      }
      output_file.close();
    }
*/
  }
}

void print_usage(char *argv0) {
  fprintf(stderr,
"\n"
"Usage: %s [OPTIONS]... NUM_CLASSES TREE_FILE LABEL_FILE OUTPUT_PREFIX\n"
"\n"
"  NUM_CLASSES              number of classes\n"
"  TREE_FILE                name of tree file\n"
"  LABEL_FILE               name of label file\n"
"  OUTPUT_PREFIX            prefix for output files\n"
"\n"
"  -S,--seed SEED           set random seed to SEED (def: 42)\n"
"  -s,--sel_type TYPE       set select type to TYPE (def: 1)\n"
"                           valid select types:\n"
"                             0  random sampling\n"
"                             1  active sampling\n"
"  -p,--period T            set output period to T (def: 100)\n"
"\n", argv0);
}

int main(int argc, char *argv[]) {

  char *tree_filename;
  char *label_filename;
  char *output_prefix;
  int sel_type = 1;
  int num_classes = 0;
  int period = 1;
  int seed = 42;

  while (1) {
    static struct option long_options[] =
    {
      {"seed", required_argument, 0, 'S'},
      {"sel_type", required_argument, 0, 's'},
      {"period", required_argument, 0, 'p'},
      {0, 0, 0, 0}
    };
    int option_index = 0;
    int c = getopt_long(argc, argv, "S:s:p:o:", long_options, &option_index);
    if (c == -1) break;
    switch (c) {
      case 0:
        break;
      case 'S':
        seed = atoi(optarg);
        break;
      case 's':
        sel_type = atoi(optarg);
        break;
      case 'p':
        period = atoi(optarg);
        break;
      default:
        print_usage(argv[0]);
        exit(-1);
    }
  }

  if (optind > argc - 4) {
    print_usage(argv[0]);
    exit(-1);
  }
  num_classes = atoi(argv[optind++]);
  tree_filename = argv[optind++];
  label_filename = argv[optind++];
  output_prefix = argv[optind++];

  Tree *tree = load_tree(tree_filename, num_classes);
  Tree *opt = load_tree(tree_filename, num_classes);
  vector<int> *labels = load_labels(label_filename);

  if (seed < 0) srand(time(0));
  else srand(seed);

  test_sampler(*tree, *opt, *labels, sel_type, period, output_prefix);

  delete tree;
  delete opt;
  delete labels;
  return 0;
}

/* EOF */