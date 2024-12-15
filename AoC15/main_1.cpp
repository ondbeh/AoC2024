#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>
#include <stdexcept>

enum direction {
	UP,
	DOWN,
	LEFT,
	RIGHT
};

enum object {
	EMPTY,
	BOX,
	WALL,
	START
};

void print_map(const std::vector<std::vector<object>> &map) {
	for (const auto &row: map) {
		for (const auto &cell: row) {
			switch (cell) {
				case EMPTY:
					std::cout << '.';
					break;
				case BOX:
					std::cout << '0';
					break;
				case WALL:
					std::cout << '#';
					break;
				case START:
					std::cout << '@';
					break;
				default:
					break;
			}
		}
		std::cout << std::endl;
	}
}

long count_GPS(const std::vector<std::vector<object>> &map) {
	long count = 0;
	for (size_t i = 0; i < map.size(); ++i) {
		for (size_t j = 0; j < map[i].size(); ++j) {
			if (map[i][j] == BOX) {
				count += i * 100 + j;
			}
		}
	}
	return count;
}

std::pair<int, int> find_start(const std::vector<std::vector<object>> &map) {
	for (size_t i = 0; i < map.size(); ++i) {
		for (size_t j = 0; j < map[i].size(); ++j) {
			if (map[i][j] == START) {
				return {i, j};
			}
		}
	}
	throw std::invalid_argument("No starting position found");
}

bool move_up(int &i, int j, std::vector<std::vector<object>> &map) {
	if (map[i][j] == WALL) {
		return false;
	}
	if (map[i][j] == EMPTY) {
		return true;
	}
	int i_up = i - 1;
	if (move_up(i_up, j, map)) {
		std::swap(map[i][j], map[i - 1][j]);
		--i;
		return true;
	}
	return false;
}

bool move_down(int &i, int j, std::vector<std::vector<object>> &map) {
	if (map[i][j] == WALL) {
		return false;
	}
	if (map[i][j] == EMPTY) {
		return true;
	}
	int i_down = i + 1;
	if (move_down(i_down, j, map)) {
		std::swap(map[i][j], map[i + 1][j]);
		++i;
		return true;
	}
	return false;
}

bool move_left(int i, int &j, std::vector<std::vector<object>> &map) {
	if (map[i][j] == WALL) {
		return false;
	}
	if (map[i][j] == EMPTY) {
		return true;
	}
	int j_left = j - 1;
	if (move_left(i, j_left, map)) {
		std::swap(map[i][j], map[i][j - 1]);
		--j;
		return true;
	}
	return false;
}

bool move_right(int i, int &j, std::vector<std::vector<object>> &map) {
	if (map[i][j] == WALL) {
		return false;
	}
	if (map[i][j] == EMPTY) {
		return true;
	}
	int j_right = j + 1;
	if (move_right(i, j_right, map)) {
		std::swap(map[i][j], map[i][j + 1]);
		++j;
		return true;
	}
	return false;
}

void start_movement(int i, int j, std::vector<std::vector<object>> &map, const std::vector<direction> &movements)
{
	for (const auto move: movements)
	{
		switch (move) {
			case UP:
				move_up(i, j, map);
				break;
			case DOWN:
				move_down(i, j, map);
				break;
			case LEFT:
				move_left(i, j, map);
				break;
			case RIGHT:
				move_right(i, j, map);
				break;
			default:
				throw std::invalid_argument("Invalid direction");
		}
	}
}


int main() {
	// Open file with improved error handling
	std::ifstream file_move("data.txt");
	std::ifstream file_map("map.txt");

	if (!file_move.is_open() || !file_map.is_open()) {
		std::cerr << "Error: Failed to open the file 'data.txt'" << std::endl;
		return 1;
	}
	std::vector<std::vector<object>> map;
	std::string line;
	std::vector<direction> movements;
	std::pair<int, int> start;

	while (std::getline(file_map, line)) {
		std::istringstream iss(line);
		std::vector<object> row;
		char c;
		while (iss >> c) {
			switch (c) {
				case '.':
					row.push_back(EMPTY);
					break;
				case 'O':
					row.push_back(BOX);
					break;
				case '#':
					row.push_back(WALL);
					break;
				case '@':
					row.push_back(START);
					break;
				default:
					break;
			}
		}
		map.push_back(row);
	}
	while(std::getline(file_move, line)) {
		std::istringstream iss(line);
		char c;
		while (iss >> c) {
			switch (c) {
				case '^':
					movements.push_back(UP);
					break;
				case 'v':
					movements.push_back(DOWN);
					break;
				case '<':
					movements.push_back(LEFT);
					break;
				case '>':
					movements.push_back(RIGHT);
					break;
				default:
					break;
			}
		}
	}
	file_map.close();
	file_move.close();
	print_map(map);

	start = find_start(map);

	start_movement(start.first, start.second, map, movements);

	long result = count_GPS(map);
	std::cout << "Result is:" << result << std::endl;
	return 0;
}