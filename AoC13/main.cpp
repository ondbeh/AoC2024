#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>

#define ADDITIONAL 50000LL

long long find_min_tokens(std::vector<std::pair<long long, long long>> settings) {
	long long pointA_x = settings[0].first, pointA_y = settings[0].second;
	long long pointB_x = settings[1].first, pointB_y = settings[1].second;
	long long prize_x = settings[2].first + ADDITIONAL;
	long long prize_y = settings[2].second + ADDITIONAL;

	long long coeffA = (prize_x * pointB_y - prize_y * pointB_x) / (pointA_x * pointB_y - pointA_y * pointB_x);
	long long coeffB = (prize_x * pointA_y - prize_y * pointA_x) / (pointB_x * pointA_y - pointB_y * pointA_x);

	long long expected_prize_x = coeffA * pointA_x + coeffB * pointB_x;
	long long expected_prize_y = coeffA * pointA_y + coeffB * pointB_y;

	if (expected_prize_x == prize_x && expected_prize_y == prize_y) {
		return 3 * coeffA + coeffB;
	}
	return 0;
}

int main() {
	std::ifstream file("data.txt");
	if (!file.is_open()) {
		std::cerr << "Failed to open the file." << std::endl;
		return 1;
	}

	std::vector<std::pair<long long, long long>> current;
	std::vector<std::vector<std::pair<long long, long long>>> settings;
	std::string line;
	long long num1, num2;
	int counter = 0;

	while (std::getline(file, line)) {
		std::istringstream iss(line);
		iss >> num1 >> num2;
		current.emplace_back(num1, num2);
		counter++;
		if (counter == 3) {
			settings.push_back(current);
			current.clear();
			counter = 0;
		}
	}
	file.close();

	long long total_tokens = 0;
	long long prizes_won = 0;

	for (const auto& curr : settings) {
		long long machine_tokens = find_min_tokens(curr);

		if (machine_tokens != 0 ) {
			total_tokens += machine_tokens;
			prizes_won++;
		}

		std::cout << "Machine - Tokens: " << machine_tokens
				  << ", A(X,Y): (" << curr[0].first << "," << curr[0].second
				  << "), B(X,Y): (" << curr[1].first << "," << curr[1].second
				  << "), Prize(X,Y): (" << curr[2].first + ADDITIONAL << "," << curr[2].second + ADDITIONAL
				  << ")" << std::endl;
	}

	std::cout << "Prizes Won: " << prizes_won << std::endl;
	std::cout << "Total Tokens: " << total_tokens << std::endl;

	return 0;
}