#include <fstream>
#include <string>
#include <iostream>
#include <set>

using Grid = std::vector<std::vector<int>>;

std::vector<std::pair<int, int>> directions = {
    {1, 0},
    {0, 1},
    {-1, 0},
    {0, -1},
};

auto sum_scores_1 = [](Grid& grid) -> int {
    int score = 0;
    std::vector<std::pair<int, int>> moves;
    // Check bounds on indexes
    auto bounds_check = [&](int i, int j) {
        return (i >= 0 && i < grid.size()) && (j >= 0 && j < grid[i].size());
    };
    // Scan all heights
    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            if (grid[i][j] != 0) continue;
            // Found a trailhead, counts its summits
            moves.push_back({ i, j });
            std::set<std::pair<int, int>> summits;
            // Move around the trail
            while (!moves.empty()) {
                auto [r, c] = moves.back();
                moves.pop_back();
                if (grid[r][c] == 9) {
                    summits.insert({ r, c });
                    continue;
                }
                // Push new directions into the stack
                for (auto &[di, dj] : directions) {
                    auto ni = r + di;
                    auto nj = c + dj;
                    // Good position worth exploring
                    if (bounds_check(ni, nj) && grid[ni][nj] - grid[r][c] == 1) {
                        moves.push_back({ ni, nj });
                    }
                }
            }
            score += summits.size();
        }
    }
    return score;
};

auto sum_scores_2 = [](Grid& grid) -> int {
    int score = 0;
    std::vector<std::pair<int, int>> moves;
    // Check bounds on indexes
    auto bounds_check = [&](int i, int j) {
        return (i >= 0 && i < grid.size()) && (j >= 0 && j < grid[i].size());
    };
    // Scan all heights
    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            if (grid[i][j] != 0) continue;
            // Found a trailhead, counts its summits
            moves.push_back({ i, j });
            int trail_score = 0;
            // Move around the trail
            while (!moves.empty()) {
                auto [r, c] = moves.back();
                moves.pop_back();
                if (grid[r][c] == 9) {
                    trail_score += 1;
                    continue;
                }
                // Push new directions into the stack
                for (auto &[di, dj] : directions) {
                    auto ni = r + di;
                    auto nj = c + dj;
                    // Good position worth exploring
                    if (bounds_check(ni, nj) && grid[ni][nj] - grid[r][c] == 1) {
                        moves.push_back({ ni, nj });
                    }
                }
            }
            score += trail_score;
        }
    }
    return score;
};

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    Grid grid;
    std::string line;
    // Read input
    while (std::getline(file, line)) {
        std::vector<int> line_ints;
        for (char &c : line) {
            line_ints.push_back(c - '0');
        }
        grid.push_back(line_ints);
    }
    int solution_a = sum_scores_1(grid);
    int solution_b = sum_scores_2(grid);

    std::cout << "Day 10" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;

    return 0;
}
