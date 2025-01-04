#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>

using Grid = std::vector<std::vector<char>>;
using Pos = std::pair<int, int>;
using Dir = std::pair<int, int>;

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    Grid grid;

    // Read input
    std::string line;
    while(std::getline(file, line)) {
        grid.push_back(std::vector(line.begin(), line.end()));
    }

    // Check guard out of map
    auto bounds_check = [&](int i, int j) {
        return i < grid.size() && i >= 0 && j < grid[i].size() && j >= 0;
    };

    // Declare all possible directions
    std::map<char, Dir> directions = {
        { '>', {0, +1} },
        { 'v', {+1, 0} },
        { '<', {0, -1} },
        { '^', {-1, 0} }
    };

    // Change direction
    auto change_dir = [](char dir) -> char {
        switch (dir) {
            case '>': return 'v';
            case 'v': return '<';
            case '<': return '^';
            case '^': return '>';
            default: return '.';
        }
    };

    // Guard checking
    auto is_guard = [](char c) {
        return c == 'v' || c == '^' || c == '>' || c == '<';
    };

    // Find the guard position within the grid
    auto find_guard = [&](Grid &grid) -> Pos {
        for (int i = 0; i < grid.size(); i++) {
            for (int j = 0; j < grid[i].size(); j++) {
                char pos = grid[i][j];
                if (is_guard(pos)) {
                    return {i, j};
                }
            }
        }
        return {};
    };

    auto simulate_guard = [&](Grid &grid) -> bool {
        auto [gx, gy] = find_guard(grid);
        // Set used to loop check
        std::set<std::tuple<int, int, char>> hit;

        while(true) {
            auto guard = grid[gx][gy];
            auto [dx, dy] = directions[guard];
            // Compute next direction
            int new_x = gx + dx;
            int new_y = gy + dy;
            // Handle out of simulation
            if (!bounds_check(new_x, new_y)) {
                grid[gx][gy] = 'X';
                return true;
            }
            // Handle obstacle
            if (grid[new_x][new_y] == '#') {
                grid[gx][gy] = change_dir(guard);
                // Check Loop only here
                if (hit.count({ gx, gy, guard }) == 1) {
                    return false;
                }
                else {
                    hit.insert({ gx, gy, guard });
                }
            }
            // Handle normal case
            else {
                grid[gx][gy] = 'X';
                gx = new_x;
                gy = new_y;
                grid[gx][gy] = guard;
            }
        }
    };

    // Solutions
    int solution_a = 0;
    int solution_b = 0;
    // Copy grid so that we can reuse the original later
    auto counted_grid = grid;
    simulate_guard(counted_grid);

    // For each visited position check with simulate_guard on a grid with an
    // obstacle placed in such position.
    for (int i = 0; i < counted_grid.size(); i++) {
        for (int j = 0; j < counted_grid[i].size(); j++) {
            bool is_counted = counted_grid[i][j] == 'X';
            solution_a += is_counted;
            if (is_counted && !is_guard(grid[i][j]) && grid[i][j] != '#') {
                // Copy grid
                auto new_grid = grid;
                // Place obstacle
                new_grid[i][j] = '#';
                auto res = !simulate_guard(new_grid);
                solution_b += res;
            }
        }
    }
    std::cout << "Day 6" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}
