#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

int main() {
	using std::begin;
	using std::end;
	using std::search;
	using std::cout;

	const int bound = 1e6+1;
	int a[bound];
	for (int i = 2; i < bound; ++i) if (!a[i]) for (int j = i * 2; j < bound; j += i) a[j] += 1;
	const int target[4] = {4,4,4,4};
	cout << search(begin(a), end(a), begin(target), end(target)) - begin(a);
}