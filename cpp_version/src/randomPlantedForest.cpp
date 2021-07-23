#include <randomPlantedForest.h>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <random>


// helper function to check whether a tree with specified split_dims already exists in tree_family
std::shared_ptr<DecisionTree> treeExists(const std::set<int> split_dims, TreeFamily &tree_family){
    for(auto& tree: tree_family){
        if(tree->get_split_dims() == split_dims){
            return tree; // if found, return pointer to tree, otherwise nullptr
        }
    }
    return nullptr;
}

// helper function to check whether a tree with resulting_dims for a split_coordinate is already in possible_splits
bool possibleExists(const int dim, const std::multimap<int, std::shared_ptr<DecisionTree>> &possible_splits, const std::set<int> &resulting_dims){
    for(auto& elem:possible_splits){
        if(elem.first == dim && elem.second->get_split_dims() == resulting_dims) return 1;
    }
    return 0;
}

// constructor
RandomPlantedForest::RandomPlantedForest(const std::vector<double> &Y, const std::vector<std::vector<double>> &X,
                                         int max_interaction, int n_trees, int n_splits, std::vector<int> n_leaves,
                                         int split_try, double t_try, std::vector<std::vector<int>> variables, bool purify_forest){
    // Check if all vector in x are same length
    for(const auto &vec:X){
        this->feature_size = vec.size();
        if(vec.size() != feature_size) std::cout << "Dimensions of X mismatch." << std::endl;
    }

    // initilize class members
    this->max_interaction = max_interaction;
    this->n_trees = n_trees;
    this->n_splits = n_splits;
    this->split_try = split_try;
    this->t_try = t_try;
    this->purify_forest = purify_forest;

    // if arguments not specified set to default
    if(n_leaves.empty()){
        this->n_leaves = std::vector<int>(feature_size, 1);
    }else{
        if(n_leaves.size() != feature_size) std::cout << "Number of nodes for leafes has wrong dimension." << std::endl;
        this->n_leaves = n_leaves;
    }
    if(variables.empty()){
        this->variables = std::vector<std::vector<int>>(feature_size);
        for(int i = 0; i<feature_size; ++i) this->variables[i] = std::vector<int> {i+1};
    }else{
        this->variables = variables;
        // todo: check for valid values in feature range
    }

    // construct tree families
    this->fit(Y, X);
}

// determine optimal split
Split RandomPlantedForest::calcOptimalSplit(const std::vector<double> &Y, const std::vector<std::vector<double>> &X,
                                            const std::multimap<int, std::shared_ptr<DecisionTree>> &possible_splits, TreeFamily &curr_family){
    Split curr_split;
    std::set<int> tree_dims;
    int k;
    double curr_sum, leaf_size, sample_point, Y_s_mean, Y_b_mean, Y_s_sum, Y_b_sum;

    // setup random device
    std::random_device rd;
    std::mt19937 gen(rd());

    // sample possible splits
    int n_candidates = ceil(t_try*possible_splits.size()); // number of candidates that will be considered
    std::vector<int> split_candidates(possible_splits.size());
    std::iota(split_candidates.begin(), split_candidates.end(), 0); // consecutive indices of possible candidates
    std::shuffle(split_candidates.begin(), split_candidates.end(), gen); // shuffle for random order

    if(false){
        std::cout << "Current candidates: (" << n_candidates << ") ";
        for(size_t n=0; n<n_candidates; ++n){
            auto candidate = possible_splits.begin();
            std::advance(candidate, split_candidates[n]);
            std::cout << candidate->first << "- ";
            for(auto dim: candidate->second->split_dims) std::cout << dim << ",";
            std::cout << "; ";
        }
        std::cout << std::endl;
    }

    // go through all trees in current family
    for(auto& curr_tree:curr_family){

        // consider a fraction of possible splits
        for(size_t n=0; n<n_candidates; ++n){

            auto candidate = possible_splits.begin();
            std::advance(candidate, split_candidates[n]); // get random split candidate without replacement

            k = candidate->first - 1; // split dim of current candidate, converted to index starting at 0
            leaf_size = n_leaves[k];

            // test if coordinate for splitting of current candidate is in current tree and results in a valid tree
            tree_dims = curr_tree->split_dims;
            tree_dims.insert(k+1); // add candidate's coordinate to current tree
            if(tree_dims != candidate->second->split_dims) continue; // if split dimensions do not match consider next candidate

            // go through all leaves of current tree
            for(auto& leaf: curr_tree->leaves){
                std::set<int> curr_individuals = leaf.individuals; // consider individuals of current leaf

                // extract sample points according to individuals from X and Y
                std::map<double, double> unique_samples;
                for(int individual: curr_individuals){
                    unique_samples[X[individual][k]] = Y[individual];
                }

                // check if number of sample points is within limit
                if(unique_samples.size() < 2*leaf_size) continue;

                // consider split_try-number of random samples
                for(int t=0; t<split_try; ++t){

                    // get samplepoint
                    auto sample_pos = unique_samples.begin();
                    std::uniform_int_distribution<> distrib(leaf_size, unique_samples.size() - leaf_size + 1);
                    std::advance(sample_pos, distrib(gen)); // consider only sample points with offset
                    sample_point = sample_pos->first;

                    std::set<int> I_s, I_b; // individuals smaller/bigger than the samplepoint
                    std::vector<double> Y_s, Y_b; // values of individuals smaller/bigger than the samplepoint

                    // sum of respective samples
                    Y_s_sum = 0;
                    Y_b_sum = 0;

                    // get samples greater/smaller than samplepoint
                    for(int individual: curr_individuals){
                        if(X[individual][k] < sample_point){
                            Y_s.push_back(Y[individual]);
                            I_s.insert(individual);
                            Y_s_sum += Y[individual];
                        }else{
                            Y_b.push_back(Y[individual]);
                            I_b.insert(individual);
                            Y_b_sum += Y[individual];
                        }
                    }

                    // get mean
                    Y_s_mean = Y_s_sum / Y_s.size();
                    Y_b_mean = Y_b_sum / Y_b.size();

                    // accumulate squared mean
                    curr_sum = 0;
                    std::for_each(Y_s.begin(), Y_s.end(), [&Y_s_mean, &curr_sum](double val){ curr_sum += pow(val - Y_s_mean, 2) + pow(val, 2); });
                    std::for_each(Y_b.begin(), Y_b.end(), [&Y_b_mean, &curr_sum](double val){ curr_sum += pow(val - Y_b_mean, 2) + pow(val, 2); });

                    if(false){
                        std::cout << "Current Min=" << curr_sum << "; ";
                        std::cout << "Means=" <<  Y_s_mean << "/" <<  Y_b_mean << std::endl;
                    }

                    // update split if squared sum is smaller
                    if(curr_sum < curr_split.min_sum){
                        curr_split.min_sum = curr_sum;
                        curr_split.tree_index = curr_tree;
                        curr_split.leaf_index =  std::make_shared<Leaf>(leaf);
                        curr_split.split_coordinate = k+1;
                        curr_split.split_point = sample_point;
                        curr_split.I_s_mean = Y_s_mean;
                        curr_split.I_b_mean = Y_b_mean;
                        curr_split.I_s = I_s;
                        curr_split.I_b = I_b;
                    }
                }
            }
        }
    }

    return curr_split;
}

// fit forest to new data
void RandomPlantedForest::fit(const std::vector<double> &Y, const std::vector<std::vector<double>> &X){

    // Check for correct input dimensions
    for(const auto &vec:X){
        this->feature_size = vec.size();
        if(vec.size() != feature_size){
			std::cout << "Dimensions of X mismatch." << std::endl;
        }
    }
    if(Y.size() != X.size()){
            std::cout << "Dimensions of X and Y mismatch." << std::endl;
    }

    this->sample_size = X.size();
    this->upper_bounds = std::vector<double>(feature_size);
    this->lower_bounds = std::vector<double>(feature_size);

    // get upper/lower bounds
    double minVal, maxVal, currVal;
    for(size_t i=0; i<feature_size; ++i){
            minVal = maxVal = X[0][i];
            for(size_t j=0; j<sample_size; ++j){
                    currVal = X[j][i];
                    if(currVal<minVal) minVal = currVal;
                    if(currVal>maxVal) maxVal = currVal;
            }
            this->upper_bounds[i] = maxVal;
            this->lower_bounds[i] = minVal;
    }

    if(false){
        std::cout << "Upper bounds: ";
        for(auto val: upper_bounds) std::cout << val << ", ";
        std::cout << "Lower bounds: ";
        for(auto val: lower_bounds) std::cout << val << ", ";
        std::cout << std::endl;
    }

    std::set<int> feature_dims;
    auto pos = feature_dims.begin();
    for(int i = 1; i <= feature_size; ++i) pos = feature_dims.insert(pos, i);

    // setup initial set of individuals
    std::set<int> initial_individuals;
    pos = initial_individuals.begin();
    for(int i = 0; i < sample_size; ++i) pos = initial_individuals.insert(pos, i);

    if(false){
        std::cout << "Initial individuals: (" << sample_size << ") ";
        for(auto val: initial_individuals) std::cout << val << ", ";
        std::cout << std::endl;
    }

    // initilize intervals with lower and upper bounds
    std::vector<Interval> initial_intervals(feature_size);
    for(size_t i = 0; i<feature_size; ++i) initial_intervals[i] = Interval{lower_bounds[i], upper_bounds[i]};

    if(false){
        std::cout << "Initial intervals: ";
        for(auto interval: initial_intervals) std::cout << interval.first << ", " << interval.second << "; ";
        std::cout << std::endl;
    }

    // set properties of first leaf
    Leaf initial_leaf;
    {
        initial_leaf.value = 0;
        initial_leaf.individuals = initial_individuals;
        initial_leaf.intervals = initial_intervals;
    }
    std::vector<Leaf> initial_leaves{initial_leaf}; // vector with initial leaf

    // store possible splits in map with splitting variable as key and pointer to resulting tree
    std::multimap<int, std::shared_ptr<DecisionTree>> possible_splits;

    // construct some helper objects
    Split curr_split;
    std::vector<std::vector<double>> samples_X = std::vector<std::vector<double>>(sample_size);
    std::vector<double> samples_Y = std::vector<double>(sample_size);

    // initilize tree families
    //n_trees = 2;
    this->tree_families = std::vector<TreeFamily>(n_trees); //(1, initial_family); // todo: change to n_trees;

    // iterate over families of trees and modify
    for(size_t n=0; n<n_trees; ++n){

        DecisionTree initial_tree;
        TreeFamily curr_family;
        for(size_t i=0; i<variables.size(); ++i){
            initial_tree.split_dims = std::set<int> (variables[i].begin(),variables[i].end());
            initial_tree.leaves = initial_leaves;
            curr_family.push_back(std::make_shared<DecisionTree>(initial_tree)); // save tree with one leaf in the beginning
        }

        if(true){
            std::cout << "Initial TreeFamily: (" << curr_family.size() << ") ";
            for(auto tree: curr_family){
                std::cout << "Dims = ";
                for(auto dim: tree->split_dims) std::cout << dim << ", ";
                std::cout << "; " << "Number of Leafs = " << tree->leaves.size();
                std::cout << " / ";
            }
            std::cout << std::endl;
        }

        // reset possible splits
        possible_splits.clear();
        for(int i=0; i<variables.size(); ++i){
            for(int j=0; j<variables[i].size(); ++j){
                // add pointer to resulting tree with split dimension as key
                possible_splits.insert(std::pair<int, std::shared_ptr<DecisionTree>>(variables[i][j], curr_family[i]));
            }
        }

        if(true){
            std::cout << "Initial Possible Splits: ";
            for(auto split: possible_splits){
                std::cout << split.first << "-";
                for(auto dim: split.second->split_dims) std::cout << dim << ",";
                std::cout << "; ";
            }
            std::cout << std::endl;
        }

        // sample data points with replacement
        int sample_index;
        for(size_t i = 0; i<sample_size; ++i){
            // todo: switch to 'std::uniform_int_distribution' for equally-likely numbers
            sample_index = std::rand() % sample_size;
            samples_Y[i] = Y[sample_index];
            samples_X[i] = X[sample_index];
        }

        // modify existing or add new trees through splitting
        for(size_t split_count=0; split_count<n_splits; ++split_count){

            // find optimal split
            curr_split = calcOptimalSplit(samples_Y, samples_X, possible_splits, curr_family);

            // continue only if we get a significant result
            if(!std::isinf(curr_split.min_sum)){

                if(true){
                    std::cout << "Current Optimal Split: " << curr_split.min_sum << "; " << curr_split.split_coordinate << "- ";
                    for(auto dim: curr_split.tree_index->split_dims) std::cout << dim << ", ";
                    std::cout << "; " << curr_split.I_s.size() << "/" << curr_split.I_b.size() << "=" << curr_split.I_s.size()+curr_split.I_b.size() << "; " <<
                                 curr_split.I_s_mean << "/" << curr_split.I_b_mean <<  std::endl;
                }

                // construct split_dims of resulting tree when splitting in split_coordninate
                std::set<int> resulting_dims = curr_split.tree_index->split_dims;
                resulting_dims.insert(curr_split.split_coordinate);
                if(resulting_dims.size() > max_interaction) continue;

                // check if resulting tree already exists in family
                std::shared_ptr<DecisionTree> found_tree = treeExists(resulting_dims, curr_family);

                // update possible_splits if max_interaction permits and not already exists
                bool found_possible = possibleExists(curr_split.split_coordinate, possible_splits, resulting_dims);
                if(!found_possible){
                    if(found_tree){ // if yes add pointer
                         possible_splits.insert(std::pair<int, std::shared_ptr<DecisionTree>>(curr_split.split_coordinate, found_tree));
                    }else{ // if not create new tree
                         curr_family.push_back(std::make_shared<DecisionTree>(DecisionTree(resulting_dims, initial_leaves)));
                         possible_splits.insert(std::pair<int, std::shared_ptr<DecisionTree>>(curr_split.split_coordinate, curr_family.back()));
                    }

                    if(true){
                        std::cout << "Updated Possible Splits: " << std::endl;
                        for(auto split: possible_splits){
                            std::cout << split.first << "-";
                            for(auto dim: split.second->split_dims) std::cout << dim << ",";
                            std::cout << "; ";
                        }
                        std::cout << std::endl;
                    }
                }

                // update values of individuals of split interval with mean
                for(int individual: curr_split.leaf_index->individuals){ // todo: indexing not correct
                    if(samples_X[individual][curr_split.split_coordinate-1] < curr_split.split_point){
                        samples_Y[individual] -= curr_split.I_s_mean;
                    }else{
                        samples_Y[individual] -= curr_split.I_b_mean;
                    }
                }

                // construct new leaves
                Leaf leaf_s, leaf_b;
                {
                    leaf_s.individuals = curr_split.I_s;
                    leaf_b.individuals = curr_split.I_b;
                    leaf_s.value = curr_split.I_s_mean;
                    leaf_b.value = curr_split.I_b_mean;

                    // initilize interval with split interval
                    leaf_s.intervals = curr_split.leaf_index->intervals;
                    leaf_b.intervals = curr_split.leaf_index->intervals;

                    // interval of leaf with smaller individuals has new upper bound in splitting dimension
                    leaf_s.intervals[curr_split.split_coordinate-1].second = curr_split.split_point;
                    // interval of leaf with bigger individuals has new lower bound in splitting dimension
                    leaf_b.intervals[curr_split.split_coordinate-1].first = curr_split.split_point;
                }

                // determine which tree is modified
                if(curr_split.tree_index->split_dims.count(curr_split.split_coordinate) ){ // if split variable is already in tree to be split
                    // change values
                    {
                        leaf_s.value = curr_split.leaf_index->value + curr_split.I_s_mean;
                        leaf_b.value = curr_split.leaf_index->value + curr_split.I_b_mean;
                    }
                    *curr_split.leaf_index = leaf_b; // replace old interval
                    curr_split.tree_index->leaves.push_back(leaf_s); // add new leaf
                } else if(found_tree){ // if tree already exists
                    found_tree->leaves.push_back(leaf_s);
                    found_tree->leaves.push_back(leaf_b);
                }else{ // create new tree

                    if(curr_family.back()->leaves.size() == 1){ // check if already added
                        curr_family.back()->leaves = std::vector<Leaf>{leaf_s, leaf_b};
                    }else{
                        curr_family.push_back(std::make_shared<DecisionTree>(DecisionTree(resulting_dims, std::vector<Leaf>{leaf_s, leaf_b})));
                    }
                }

                if(true){
                    std::cout << "Current TreeFamily: (" << curr_family.size() << ") ";
                    for(auto tree: curr_family){
                        std::cout << "Dims = ";
                        for(auto dim: tree->split_dims) std::cout << dim << ", ";
                        std::cout << "; " << "Number of Leafs = " << tree->leaves.size();
                        std::cout << " / ";
                    }
                    std::cout << std::endl << std::endl;
                }
            }
        }

        // todo: clear individuals of each tree
        // todo: remove empty trees

        // optional: purify tree
        if(purify_forest){
            this->purify();
        }else{
            purified = false;
        }

        tree_families[n] = curr_family;
    }
}

// predict single feature vector
double RandomPlantedForest::predict(const std::vector<double> &X){

    int component_index = 0;
    double total_res = 0;

    for(auto& tree_family: this->tree_families){
        for(auto& tree: tree_family){
            for(auto& leaf: tree->leaves){
                for(auto& dim: tree->split_dims){
                    if(leaf.intervals[dim-1].first <= X[dim-1]
                       && (leaf.intervals[dim-1].second > X[dim-1]
                       || leaf.intervals[dim-1].second == upper_bounds[dim-1])){
                        total_res += leaf.value;
                    }
                }
            }
        }
    }
    double average_res = total_res / n_trees;

    return average_res;
}

// predict multiple feature vectors
std::vector<double> RandomPlantedForest::predict(const std::vector<std::vector<double>> &X){
    if(X.empty()) {std::cout << "X must not be empty." << std::endl; return std::vector<double>{};}
    if(X[0].size() != feature_size) {
        std::cout << "X has wrong dimension. " << X[0].size() << " vs. " << feature_size << std::endl;
        return std::vector<double>{};
    }

    std::vector<double> res;
    for(auto& feature_vec: X){
        res.push_back(this->predict(feature_vec));
    }
    return res;
}

//
void RandomPlantedForest::purify(){
    purified = true;
}


std::vector<TreeFamily> RandomPlantedForest::get_forest(){
    return this->tree_families;
}
