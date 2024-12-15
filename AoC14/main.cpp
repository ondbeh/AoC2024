#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>
#include <stdexcept>

const int SPACE_WIDTH = 101;
const int SPACE_HEIGHT = 103;
const int QUADRANT_SPLIT_X = 50;
const int QUADRANT_SPLIT_Y = 51;
const int SIMULATION_TIME = 10000;

int getQuadrant(int x, int y) {
	if (x < QUADRANT_SPLIT_X && y < QUADRANT_SPLIT_Y) return 1;
	if (x < QUADRANT_SPLIT_X && y > QUADRANT_SPLIT_Y) return 2;
	if (x > QUADRANT_SPLIT_X && y < QUADRANT_SPLIT_Y) return 3;
	if (x > QUADRANT_SPLIT_X && y > QUADRANT_SPLIT_Y) return 4;
	return 0;
}

void move_robots(std::vector<std::pair<std::pair<int, int>, std::pair<int, int>>> &settings) {
	for (auto &robot: settings) {
		int x = robot.first.first;
		int y = robot.first.second;
		int vx = robot.second.first;
		int vy = robot.second.second;
		x += vx;
		y += vy;
		x = (x + SPACE_WIDTH) % SPACE_WIDTH;
		y = (y + SPACE_HEIGHT) % SPACE_HEIGHT;
		robot.first.first = x;
		robot.first.second = y;
	}
}

bool check_for_bundle(std::vector<std::pair<std::pair<int, int>, std::pair<int, int>>> &settings){
	float bundle_perc = 0.2f;
	int bundle_space = 15;
	int bundle_count = 0;
	std::pair<std::pair<int, int>, std::pair<int, int>> prev = {{1, -1}, {-1, -1}};

	for (auto robot: settings)
	{
		if (prev.first != (std::pair<int, int>){-1, -1})
		{
			int x1 = prev.first.first;
			int y1 = prev.first.second;
			int x2 = robot.first.first;
			int y2 = robot.first.second;
			if (std::abs(x1 - x2) <= bundle_space && std::abs(y1 - y2) <= bundle_space)
			{
				bundle_count++;
			}
		}
		prev = robot;
	}
	if (bundle_count >= settings.size() * bundle_perc)
		return true;
	return false;
}


int main() {
	// Open file with improved error handling
	std::ifstream file("data.txt");
	if (!file.is_open()) {
		std::cerr << "Error: Failed to open the file 'data.txt'" << std::endl;
		return 1;
	}
	std::vector<std::pair<std::pair<int, int>, std::pair<int, int>>> settings;
	std::string line;
	long long num1, num2, num3, num4;

	while (std::getline(file, line)) {
		std::istringstream iss(line);
		iss >> num1 >> num2 >> num3 >> num4;
		settings.push_back({{num1, num2}, {num3, num4}});
	}
	file.close();
	std::cout << "Initial Robot Settings:" << std::endl;
	for (size_t i = 0; i < settings.size(); ++i) {
		std::cout << "Robot " << i << ": "
				  << "pos=(" << settings[i].first.first << "," << settings[i].first.second << ") "
				  << "vel=(" << settings[i].second.first << "," << settings[i].second.second << ")" << std::endl;
	}
	for (int i = 1; i <= SIMULATION_TIME; ++i)
	{
		move_robots(settings);
		if (check_for_bundle(settings))
		{
			std::cout << "Bundle found at time " << i << std::endl;
			break;
		}
	}
	int q1 = 0, q2 = 0, q3 = 0, q4 = 0;
	for (const auto& pos : settings) {
		int quadrant = getQuadrant(pos.first.first, pos.first.second);
		switch(quadrant) {
			case 1: q1++; break;
			case 2: q2++; break;
			case 3: q3++; break;
			case 4: q4++; break;
		}
	}

	// Calculate and print safety factor
	std::cout << "Quadrant Counts: Q1=" << q1 << ", Q2=" << q2 << ", Q3=" << q3 << ", Q4=" << q4 << std::endl;
	std::cout << "Safety Factor: " << q1 * q2 * q3 * q4 << std::endl;

	return 0;
}