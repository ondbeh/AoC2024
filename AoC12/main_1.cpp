#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <queue>

struct node{
	char crop;
	bool visited;
};

int amount_of_neighbors(std::vector<std::vector<node>> &grid, size_t i, size_t j){
	int count = 0;
	if (i > 0 && grid[i - 1][j].crop == grid[i][j].crop){
		count++;
	}
	if (i < grid.size() - 1 && grid[i + 1][j].crop == grid[i][j].crop){
		count++;
	}
	if (j > 0 && grid[i][j - 1].crop == grid[i][j].crop){
		count++;
	}
	if (j < grid[0].size() - 1 && grid[i][j + 1].crop == grid[i][j].crop){
		count++;
	}
	return count;
}

int go_through(std::vector<std::vector<node>> &grid, size_t i, size_t j){
    std::queue<std::pair<size_t, size_t>> q;
    q.push({i, j});
    int area = 0;
    int perimeter = 0;
    char starting_crop = grid[i][j].crop;

    while (!q.empty()) {
        auto [ci, cj] = q.front();
        q.pop();
        node &current = grid[ci][cj];
        if (current.visited || current.crop != starting_crop){
            continue;
        }
        current.visited = true;
        area++;
        if (ci > 0 && !grid[ci - 1][cj].visited){
            q.push({ci - 1, cj});
        }
        if (ci < grid.size() - 1 && !grid[ci + 1][cj].visited){
            q.push({ci + 1, cj});
        }
        if (cj > 0 && !grid[ci][cj - 1].visited){
            q.push({ci, cj - 1});
        }
        if (cj < grid[0].size() - 1 && !grid[ci][cj + 1].visited){
            q.push({ci, cj + 1});
        }
        perimeter += 4 - amount_of_neighbors(grid, ci, cj);
    }
    return area * perimeter;
}

int main(void)
{
	std::ifstream file("input.txt");
	std::string line;
	std::vector<std::vector<node>> grid;

	while (std::getline(file, line)){
		std::vector<node> row;
		for (char c : line){
			row.push_back({c, false});
		}
		grid.push_back(row);
	}

	for (auto row : grid){
		for (auto n : row){
			std::cout << n.crop;
		}
		std::cout << std::endl;
	}
	int result = 0;

	for (size_t i = 0; i < grid.size(); i++)
	{
		for (size_t j = 0; j < grid[0].size(); j++)
		{
			if (grid[i][j].visited){
				continue;
			}
			std::cout << "going through area at " << i << ", " << j << std::endl;
			result += go_through(grid, i, j);
		}
	}
	std::cout << result << std::endl;
	return 0;
}
