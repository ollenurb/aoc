#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

enum Operator {
    STAR = 0,
    PLUS = 1,
    PIPE = 2
};

using Sequence = std::vector<u_int32_t>;
using Operators = std::vector<Operator>;

auto evaluate = [](Sequence &seq, std::vector<int> &ops) -> u_int64_t {
    u_int64_t result = seq.front();
    for (int i = 0; i < ops.size(); i++) {
        switch (ops[i]) {
            case STAR: {
                result *= seq[i + 1]; 
                break;
            }
            case PLUS: {
                result += seq[i + 1]; 
                break;
            }
            case PIPE: {
                auto res = std::to_string(result) + std::to_string(seq[i + 1]);
                result = stoull(res);
                break;
            }
            default: {
                break;
            }
        }
    }
    return result;
};

// Parse the string to a pair of num and sequence
auto parse_string = [](std::string& line) -> std::pair<u_int64_t, Sequence> {
    std::stringstream ssline (line);
    std::vector<u_int32_t> sequence;
    char c;
    u_int64_t result = 0 ;
    ssline >> result >> c;
    if (c != ':') {
        throw std::invalid_argument("Input string is not formatted correctly.");
    }

    u_int32_t val = 0;
    while (ssline >> val) {
        sequence.push_back(val);
    }
    
    return {result, sequence};
};

// Check validity of the sequence 
auto check_validity = [](
    std::pair<u_int64_t,
    std::vector<u_int32_t>>& sequence,
    bool include_pipe
) -> bool {
    auto [result, seq] = sequence;
    // No. of possible operations
    int N_OPS = include_pipe ? 3 : 2;
    int seq_len = seq.size() - 1;
    // Store the current combination in index form
    std::vector<int> combination(seq_len, 0);

    while (true) {
        // Create a vector to store the current combination of enum values
        if (evaluate(seq, combination) == result) {
            return true;
        }

        // Increment the indices to generate the next combination
        int position = seq_len - 1;
        while (position >= 0 && combination[position] == N_OPS - 1) {
            combination[position] = 0;
            position -= 1;
        }
        // Exhausted combinations case
        if (position < 0) break;

        // Increment the position
        combination[position] += 1;
    }
    return false;
};


int main(int argc, char** argv) {
    std::ifstream file(argv[1]);

    u_int64_t solution_a = 0;
    u_int64_t solution_b = 0;

    std::string line;
    while (std::getline(file, line)) {
        auto parsed = parse_string(line);
        if (check_validity(parsed, false)) {
            solution_a += parsed.first;
        }
        if (check_validity(parsed, true)) {
            solution_b += parsed.first;
        }
    }
    
    std::cout << "Day 7" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}