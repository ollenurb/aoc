#include <iostream>
#include <fstream>
#include <vector>
#include <tuple>
#include <string>
#include <algorithm>
#include <cmath>

int main(int argc, char** argv) {
    std::vector<int> first_list;
    std::vector<int> second_list;

    // Read file and store its parsed content into a vector
    std::ifstream file(argv[1]);
    int num;
    bool is_first = true;
    while (file >> num) {
        if (is_first) {
            first_list.push_back(num);
            is_first = !is_first;
        } else {
            second_list.push_back(num);
            is_first = !is_first;
        }
    }
    
    // Solve "a"
    std::sort(first_list.begin(), first_list.end());
    std::sort(second_list.begin(), second_list.end());
    
    unsigned long solution_a = 0;
    for (int i = 0; i < first_list.size(); i++) {
        solution_a += std::abs(first_list[i] - second_list[i]);
    }
    
    // Solve "b"
    unsigned long solution_b = 0;
    for (int i = 0; i < first_list.size(); i++) {
        for (int j = 0; j < second_list.size(); j++) {
            if (second_list[j] > first_list[i]) {
                // stop searching further
                break;
            }
            if (second_list[j] == first_list[i]) {
                solution_b += first_list[i];
            }
        }
    }

    // Print results
    std::cout << "Day 1" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}
