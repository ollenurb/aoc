#include <iostream>
#include <fstream>
#include <optional>
#include <map>

// Parses the mul(num_1, num_2) operator using automata based parsing, then
// returns 0 if it doesn't parse anything, otherwise it returns the product
// between the parsed arguments. 
int try_exec_mul(std::ifstream& file) {
    enum MulAutomataState {
        EXP_M = 0, // START state
        EXP_U = 1,
        EXP_L = 2,
        EXP_LPAR = 3,
        EXP_LDIGIT = 4, 
        EXP_RDIGIT = 5,
        END = 6,
    };

    // Define state transitions of the MUL Automata
    std::vector<std::map<char, MulAutomataState>> mul_state_transition = {
        { {'m', EXP_U} }, // EXP_M
        { {'u', EXP_L} }, // EXP_U
        { {'l', EXP_LPAR} }, // EXP_L
        { {'(', EXP_LDIGIT} }, // EXP_LPAR
        { {',', EXP_RDIGIT} }, // EXP_LDIGIT
        { {')', END} }, // EXP_LDIGIT
        {} // STOP
    };
    MulAutomataState state = EXP_M;
    std::string num_1, num_2; 
    while (true) {
        char peek = file.peek();
        switch(state) {
            case MulAutomataState::EXP_LDIGIT: {
                if (std::isdigit(peek)) {
                    num_1 += peek;
                    file.get();
                    continue;
                }
                break;
            }
            case MulAutomataState::EXP_RDIGIT: {
                if (std::isdigit(peek)) {
                    num_2 += peek;
                    file.get();
                    continue;
                }
                break;
            }
            case MulAutomataState::END: {
                return std::stoi(num_1) * std::stoi(num_2);
            }
            default: break;
        }
        // Check if the map contains this character
        auto state_map = mul_state_transition[state];
        if (state_map.find(peek) != state_map.end()) {
            state = state_map[peek];
            file.get();
        }
        else {
            // No transition contained, return 0
            return 0;
        }
    }
}

std::optional<bool> try_exec_do(std::ifstream& file) {
    enum DoAutomataState {
        EXP_D,
        EXP_O,    
        EXP_APOS,
        EXP_T,
        EXP_LPAR_OR_N,
        EXP_LPAR,
        EXP_RPAR,
        END
    };

    // Define state transitions of the MUL Automata
    std::vector<std::map<char, DoAutomataState>> do_state_transition = {
        { {'d', EXP_O} }, // EXP_D
        { {'o', EXP_LPAR_OR_N} }, // EXP_O
        { {'\'', EXP_T} }, // EXP_APOS
        { {'t', EXP_LPAR} }, // EXP_T
        { {'n', EXP_APOS}, {'(', EXP_RPAR} }, // EXP_LPAR_OR_N
        { {'(', EXP_RPAR} }, // EXP_LPAR
        { {')', END} }, // EXP_RPAR
    };
    DoAutomataState state = EXP_D;
    bool is_do = true;
    while (true) {
        char peek = file.peek();
        switch(state) {
            case DoAutomataState::EXP_APOS: {
                is_do = false;
                break;
            }
            case DoAutomataState::END: {
                return std::make_optional(is_do);
            }
            default: break;
        }
        // Check that the transition map contains this character
        auto state_map = do_state_transition[state];
        if (state_map.find(peek) != state_map.end()) {
            state = state_map[peek];
            file.get();
        }
        else {
            // No transition contained, return an empty optional
            return {};
        }
    }
}

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    int solution_a = 0;
    int solution_b = 0;

    bool is_active = true;
    char peek = file.peek();
    // Read file char by char, when encountering either 'm'or 'd'try to parse
    // the corresponding instruction
    while (peek != EOF) {
        switch(peek) {
            case 'm': {
                int mul_res = try_exec_mul(file);
                solution_a += mul_res;
                solution_b += is_active ? mul_res : 0;
                break;
            }
            case 'd': {
                auto do_res = try_exec_do(file);
                if (do_res.has_value()) {
                    is_active = do_res.value();
                }
                
                break;
            }
            default: {
                file.get();
                break;
            }
        }
        peek = file.peek();
    }

    std::cout << "Day 3" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}