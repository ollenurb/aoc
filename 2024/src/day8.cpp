#include <fstream>
#include <map>
#include <string>
#include <iostream>
#include <set>

using Grid = std::vector<std::vector<char>>;
using Pos = std::pair<int, int>;
// Store the location of all the antennas
using Antennas = std::map<char, std::set<Pos>>;

// Return antennas sorted by antenna type
auto find_antennas = [](Grid& grid) -> Antennas {
    Antennas antennas;
    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            if (grid[i][j] != '.') {
                antennas[grid[i][j]].insert({i, j});
            }
        }
    }
    return antennas;
};

auto bounds_check = [](Grid& grid, int ax, int ay) {
    return ax >= 0 && ax < grid.size() && ay >= 0 && ay < grid[ax].size();
};

// Given all antennas, create all antinodes
auto get_antinodes_a = [](Antennas& antennas, Grid& grid) -> std::set<Pos> {
    std::set<Pos> result;
    // Loop through all antenna types
    for (const auto& [_, positions] : antennas) {
        // Loop through all antenna positions of a specific type
        for (const auto& [px, py] : positions) {
            // Loop (again) on the same antenna type positions
            for (const auto& [ax, ay] : positions) {
                if (px == ax && py == ay) continue; // Same node, skip
                // Compute antinode
                int nx = ax + (ax - px);
                int ny = ay + (ay - py);
                // Bounds check
                if (bounds_check(grid, nx, ny)) {
                    result.insert({nx, ny});
                }
            }
        }
    }
    return result;
};

auto get_antinodes_b = [](Antennas& antennas, Grid& grid) -> std::set<Pos> {
    std::set<Pos> result;
    // Loop through all antenna types
    for (const auto& [_, positions] : antennas) {
        // Loop through all antenna positions of a specific type
        for (const auto& [px, py] : positions) {
            // Loop (again) on the same antenna type positions
            for (const auto& [ax, ay] : positions) {
                if (px == ax && py == ay) continue; // Same node, skip
                // Compute antinode
                int nx = px + (ax - px);
                int ny = py + (ay - py);
                // Continue generating until out of grid
                while (bounds_check(grid, nx, ny)) {
                    result.insert({nx, ny});
                    nx = nx + (ax - px);
                    ny = ny + (ay - py);
                }
            }
        }
    }
    return result;
};

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    Grid grid;
    std::string line;
    // Read input
    while (std::getline(file, line)) {
        grid.push_back(std::vector(line.begin(), line.end()));
    }
    // Find all antennas
    auto antennas = find_antennas(grid);

    // Generate antinodes
    auto antinodes_a = get_antinodes_a(antennas, grid);
    auto antinodes_b = get_antinodes_b(antennas, grid);

    std::cout << "Day 8" << std::endl;
    std::cout << "Solution a: " << antinodes_a.size() << std::endl;
    std::cout << "Solution b: " << antinodes_b.size() << std::endl;

    return 0;
}
