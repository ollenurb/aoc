#include <cstdlib>
#include <cstdio>
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <tuple>
#include <map>


std::vector<int> parse_ints(std::string& input) {
    std::vector<int> result;
    std::string num;
    for (char c : input) {
        if (c == ',') {
            result.push_back(stoi(num));
            num.clear();
        } else {
            num += c;
        }
    } 
    // last number is not separated by anything
    if (!num.empty()) {
        result.push_back(stoi(num));
    }
    return result;
}

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    std::map<int, std::vector<int>> rules;
    std::vector<std::vector<int>> updates;

    // Parse rules 
    std::string line;
    while(std::getline(file, line) && !line.empty()) {
        size_t pos = line.find('|');
        int before_num = stoi(line.substr(0, pos));
        int after_num = stoi(line.substr(pos + 1));
        rules[before_num].push_back(after_num);
    }

    while(std::getline(file, line)) {
        updates.push_back(parse_ints(line));
    }

    // Solve b
    auto check_sanity = [&](std::vector<int> &update) {
        for (int i = 0; i < update.size(); i++) {
            int cur_val = update[i];
            // For each rule, check in the corresponding subvector if the rule
            // is violated
            for (auto &rule : rules[cur_val]) {
                for (int j = i; j >= 0; j--) {
                    if (update[j] == rule) {
                        return false;
                    }
                }
            }
        }
        return true;
    };

    auto move_after = [&](std::vector<int> &update, int j, int i) {
        int tmp = update[j]; // = 2
        for (int k = j; k < i; k++) {
            update[k] = update[k + 1];
        }
        update[i] = tmp;
    };

    auto correct_update = [&](std::vector<int> &update) {
        for (int i = 0; i < update.size(); i++) {
            int cur_val = update[i];
            // For each rule, check in the corresponding subvector if the rule
            // is violated
            for (auto &rule : rules[cur_val]) {
                // Check subsequence
                for (int j = i; j >= 0; j--) {
                    // We found a rule that breaks, just put the element after
                    if (update[j] == rule) {
                        move_after(update, j, i);
                        i--;
                    }
                }
            }
        }
    };

    // Solve a 
    int solution_a = 0;
    int solution_b = 0;
    for (auto &update : updates) {
        int half = update.size() / 2;
        if (check_sanity(update)) {
            solution_a += update[half];
        }
        else {
            correct_update(update);
            solution_b += update[half];
        }
    }

    std::cout << "Day 5" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}