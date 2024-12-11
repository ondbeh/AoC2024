#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <unordered_map>

#define MAX_DEPTH 500

unsigned int fastLength(unsigned long num) {
    unsigned int length = 0;
    do {
        num /= 10;
        ++length;
    } while (num > 0);
    return length;
}

unsigned long ft_pow(unsigned long base, unsigned long exp){
	unsigned long result = 1;
	for (unsigned long i = 0; i < exp; i++){
		result *= base;
	}
	return result;
}

unsigned long long recalc_stones(std::vector<unsigned long> stones, std::unordered_map<unsigned long, unsigned long> *memo, int depth){
	unsigned int length = 0;
	unsigned long long count = 0;
	std::vector<unsigned long> new_stones;

	for (unsigned long i = 0; i < stones.size(); i++){
		if (stones[i] == 0){
			new_stones.push_back(1);
		}
		else if ( (length = fastLength(stones[i])) % 2 == 0){
			new_stones.push_back(stones[i] / ft_pow(10, length / 2));
			new_stones.push_back(stones[i] % ft_pow(10, length / 2));
		}
		else
		{
			new_stones.push_back(stones[i] * 2024);
		}
	}
	if (depth < MAX_DEPTH - 1){
		for (auto stone : new_stones){
			if (memo[depth].find(stone) == memo[depth].end()){
				memo[depth][stone] = recalc_stones(std::vector<unsigned long> {stone}, memo, depth + 1);
			}
			count += memo[depth][stone];
		}
	}
	else{
		count = new_stones.size();
	}
	return count;
}


int main(void){
	std::ifstream file("input.txt");
	std::string line;
	std::vector<unsigned long> stones;

	unsigned long curr;

	std::getline(file, line);
	std::istringstream line_stream(line);
	std::unordered_map<unsigned long, unsigned long> memo[MAX_DEPTH];

	while (line_stream >> curr){
		stones.push_back(curr);
	}
	unsigned long long count = recalc_stones(stones, memo, 0);

	std::cout << count << std::endl;


}
