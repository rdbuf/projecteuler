#include <iostream>
#include <unordered_map>
#include <cmath>
#include <algorithm>

int main() {
	const int bound = 1000;

	using std::unordered_map;
	using std::pow;
	using std::max_element;
	using std::cout;

	unordered_map<unsigned, unsigned> s;
	for (int i = 1; i <= bound; ++i) // a
		for (int j = i; j <= bound; ++j) // b
			for (int k = j; i + j + k <= bound; ++k) // hypotenuse
				if (pow(i, 2) + pow(j, 2) == pow(k, 2))
					s[i + j + k] += 1;
	cout << max_element(s.begin(), s.end(), [](auto& a, auto& b) { return a.second < b.second; })->first;
}