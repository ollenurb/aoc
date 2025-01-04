#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>

bool is_sorted(std::vector<int>& sequence) {
    bool ascending = true;
    bool descending = true;
    for (int i = 0; i < sequence.size() - 1; i++) {
        auto diff = sequence[i] - sequence[i + 1];
        bool diff_ok = std::abs(diff) >= 1 && std::abs(diff) <= 3;
        ascending = ascending && diff <= 0 && diff_ok;
        descending = descending && diff >= 0 && diff_ok;
    }
    return ascending || descending;
}

bool is_sorted_tolerant(std::vector<int>& sequence) {
    for (int i = 0; i < sequence.size(); i++) {
        std::vector test_seq = std::vector(sequence);
        test_seq.erase(test_seq.begin() + i);
        if (is_sorted(test_seq)) return true;
    }
    return false;
}

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);

    // Solve "a"
    unsigned int solution_a = 0;
    unsigned int solution_b = 0;
    for (std::string line; std::getline(file, line);) {
        std::stringstream line_stream(line);
        std::vector<int> report;
        unsigned int num = 0;
        while (line_stream >> num) {
            report.push_back(num);
        }
        // Count solutions
        solution_a += is_sorted(report);
        solution_b += is_sorted_tolerant(report);
    }

    std::cout << "Day 2" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}
