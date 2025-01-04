#include <fstream>
#include <vector>
#include <iostream>

using FileSystem = std::vector<int>;

struct Chunk {
    int id = -1;
    int size = 0;
};

auto compact_sectors = [](FileSystem& fsystem) {
    int j = 0;
    int i = fsystem.size() - 1;
    while (true) {
        // Position the file reader at the first file chunk
        while (fsystem[i] == -1 && i >= 0 && i > j) {
            i--;
        }
        // Position the file writer at the first empty chunk
        while (fsystem[j] != -1 && j < fsystem.size() && j < i) {
            j++;
        }
        // Finish
        if (i <= j || i < 0 || j >= fsystem.size()) break;
        // Update positions
        fsystem[j] = fsystem[i];
        fsystem[i] = -1;
    }
};

auto chunkify = [](FileSystem& fsystem) -> std::vector<Chunk> {
    std::vector<Chunk> chunks;
    Chunk chunk = { fsystem[0], 1 };

    for (int i = 1; i <= fsystem.size(); i++) {
        // Found another chunk..
        if (i == fsystem.size() || fsystem[i] != chunk.id) {
            chunks.emplace_back(chunk);
            // Reset conuters if not at the end
            if (i < fsystem.size()) {
                chunk.id = fsystem[i];
                chunk.size = 1;
            }
        }
        else {
            chunk.size += 1;
        }
    }
    return chunks;
};

auto compact_files = [](std::vector<Chunk>& chunks) {
    // Find the first empty chunk
    for (int i = 0; i < chunks.size(); i++) {
        if (chunks[i].id == -1) {
            // Find the next file
            for (int j = chunks.size() - 1; j >= i; j--) {
                // File fits
                if (chunks[j].id != -1 && chunks[i].size >= chunks[j].size) {
                    chunks[i].size = chunks[i].size - chunks[j].size;
                    int tmp = chunks[j].id;
                    chunks[j].id = -1;
                    chunks.insert(chunks.begin() + i, { tmp, chunks[j].size });
                    i--;
                    // Becomes empty space
                    break;
                }
            }
        }
    }
};

auto checksum_sectors = [](FileSystem& fsystem) -> u_int64_t {
    u_int64_t res = 0;
    for (int i = 0; i < fsystem.size(); i++) {
        if (fsystem[i] != -1) {
            res += i * fsystem[i];
        }
    }
    return res;
};

auto checksum_chunks = [](std::vector<Chunk>& chunks) -> u_int64_t {
    u_int64_t res = 0;
    int i = 0;
    for (auto& chunk : chunks) {
        for (int k = 0; k < chunk.size; k++) {
            if (chunk.id != -1) {
                res += i * chunk.id;
            }
            i++;
        }
    }
    return res;
};

int main(int argc, char** argv) {
    std::ifstream file(argv[1]);
    FileSystem fsystem;
    char buffer[2];
    unsigned int fid = 0;
    while(file.read(buffer, 2)) {
        int file = buffer[0] - '0';
        int empty = (buffer[1] != '\n') ? buffer[1] - '0' : 0;
        // Fill file chunks
        for (int i = 0; i < file; i++) {
            fsystem.push_back(fid);
        }
        // Fill empty chunks
        for (int i = 0; i < empty; i++) {
            fsystem.push_back(-1);
        }
        fid += 1;
    }
    // Copy the filesystem
    FileSystem original_fs = fsystem;

    u_int64_t solution_a = 0;
    u_int64_t solution_b = 0;
    // Solution a
    compact_sectors(fsystem);
    solution_a = checksum_sectors(fsystem);

    // Solution b
    auto chunks = chunkify(original_fs);
    compact_files(chunks);
    solution_b = checksum_chunks(chunks);

    std::cout << "Day 9" << std::endl;
    std::cout << "Solution a: " << solution_a << std::endl;
    std::cout << "Solution b: " << solution_b << std::endl;
}
