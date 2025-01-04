#include <string>
#include <iostream>
#include <fstream>
#include <vector>

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    std::vector<std::vector<char>> fcontent;

    // Read file
    std::string line;
    while(std::getline(file, line)) {
        fcontent.push_back(std::vector<char>(line.begin(), line.end()));
    }

    // Get size
    int ROW_SIZE = fcontent.size();
    int COL_SIZE = fcontent.front().size();

    // Solve part a
    int solution_a = 0;

    auto bounds_check = [&](int i, int j, int di, int dj) {
        return (i + di * 3 >= 0 && i + di * 3 < ROW_SIZE) && (j + dj * 3 >= 0 && j + dj * 3 < COL_SIZE);
    };

    auto check_xmas_a = [&](int i, int j, int di, int dj) {
        return (
            fcontent[i + di][j + dj] == 'M' &&
            fcontent[i + di * 2][j + dj * 2] == 'A' &&
            fcontent[i + di * 3][j + dj * 3] == 'S'
        );
    };

    for (int i = 0; i < ROW_SIZE; i++) {
        for (int j = 0; j < COL_SIZE; j++) {
            for (int di = -1; di <= 1; di++) {
                for (int dj = -1; dj <= 1; dj++) {
                    if (fcontent[i][j] != 'X') continue;
                    if (dj == 0 && di == 0) continue;
                    if (!bounds_check(i, j, di, dj)) continue;
                    if (check_xmas_a(i, j, di, dj)) {
                        solution_a += 1;
                    }
                }
            }
        }
    }

    // Solve part b
    std::vector<std::vector<int>> vertices = {
        {-1, -1, +1, -1, +1, +1, -1, +1},
        {+1, -1, +1, +1, -1, +1, -1, -1},
        {+1, +1, -1, +1, -1, -1, +1, -1},
        {-1, +1, -1, -1, +1, -1, +1, +1},
    };

    // Check all rotations of vertices
    auto check_xmas_b = [&](int i, int j) {
        for (auto &v : vertices) {
            auto m_1 = fcontent[i + v[0]][j + v[1]];
            auto m_2 = fcontent[i + v[2]][j + v[3]];
            auto s_1 = fcontent[i + v[4]][j + v[5]];
            auto s_2 = fcontent[i + v[6]][j + v[7]];
            if (m_1 == 'M' && m_2 == 'M' && s_1 == 'S' && s_2 == 'S') {
                return true;
            }
        }
        return false;
    };

    int solution_b = 0;

    for (int i = 1; i < ROW_SIZE - 1; i++) {
        for (int j = 1; j < COL_SIZE - 1; j++) {
            if (fcontent[i][j] != 'A') continue;
            if (check_xmas_b(i, j)) {
                solution_b += 1;
            }

        }
    }

    std::cout << "Day 4" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}
