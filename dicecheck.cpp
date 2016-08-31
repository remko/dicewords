#include <boost/regex.hpp>
#include <fstream>
#include <functional>
#include <iostream>
#include <climits>
#include <sstream>
#include <vector>
#include <string>
#include <cmath>
#include <unordered_set>
#include <algorithm>
#include <utility>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm_ext/push_back.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm/find_if.hpp>
#include <boost/range/algorithm/adjacent_find.hpp>
#include <boost/range/algorithm.hpp>

using namespace boost;
using namespace boost::algorithm;
using namespace boost::adaptors;

bool isComposite(
		const std::string& word, 
		const std::vector<std::string>& wordsByLength,
		const std::unordered_set<std::string>& words, 
		int maxLength) {
	return wordsByLength.end() != find_if(wordsByLength, [&](const auto& nextWord) {
		std::string newWord = word + nextWord;
		if (newWord.size() > maxLength) {
			return false;
		}
		else if (words.find(newWord) != words.end()) {
			std::cerr << "'" << newWord << "' is composite. ";
			return true;
		}
		else if (newWord.size() < maxLength && isComposite(newWord, wordsByLength, words, maxLength)) {
			return true;
		}
		else {
			return false;
		}
	});
}

std::vector<std::string> readLines(const std::string& filename) {
	std::vector<std::string> lines;
	std::ifstream infile(filename);
	std::string line;

	auto inMessage = false;
	auto expectHash = false;
	while (std::getline(infile, line)) {
		trim(line);
		if (regex_search(line, regex("^-----BEGIN PGP SIGNED MESSAGE"))) {
			inMessage = true;
			expectHash = true;
		}
		else if (regex_search(line, regex("^-----BEGIN PGP SIGNATURE"))) {
			inMessage = false;
		}
		else if (expectHash) {
			expectHash = false;
		}
		else if (lines.empty() || inMessage) {
			inMessage = true;
			if (!line.empty() && !starts_with(line, "#")) {
				lines.push_back(line);
			}
		}
	}
	return lines;
}

bool errors = false;
void check(const std::string& message, const std::function<bool()>& f) {
	std::cerr << message << ": " << std::flush;
	auto result = f();
	std::cerr << (result ? "ok" : "not ok") << std::endl;
	if (!result) {
		errors = true;
	}
}

int main(int argc, const char* argv[]) {
	auto lines = readLines(argv[1]);

	std::vector<std::string> words;
	std::vector<std::string> rolls;
	bool mode8k;
	if (regex_match(lines[0], regex("^\\d\\d\\d\\d\\d\\s+.+$"))) {
		mode8k = false;
		push_back(words, lines | transformed([](const auto& w) {
			return w.substr(6);
		}));
		push_back(rolls, lines | transformed([](const auto& w) {
			return w.substr(0, 5);
		}));
	}
	else {
		mode8k = true;
		words = lines;
	}
	sort(words);

	auto minMaxLength = accumulate(words, std::make_pair(INT_MAX, 0), [](const auto& acc, const auto& word) {
		return std::make_pair(
			std::min((int) word.size(), acc.first),
			std::max((int) word.size(), acc.second)
		);
	});
	auto averageLength = accumulate(words, 0, [](const auto& acc, const auto& w) {
		return acc + w.size();
	}) / (double) words.size();

	std::cerr << "Format: " << (mode8k ? "8k" : "dice") << std::endl;
	std::cerr << "Minimum word length: " << minMaxLength.first << std::endl;
	std::cerr << "Maximum word length: " << minMaxLength.second << std::endl;
	std::cerr << "Average word length: " << averageLength << std::endl;
	std::cerr << "Contains spaces: " << (words.end() != find_if(words, [](const auto& w) {
		return regex_search(w, regex("\\s"));
	}) ? "yes" : "no") << std::endl;
	std::cerr << "Contains numbers: " << (words.end() != find_if(words, [](const auto& w) {
		return regex_search(w, regex("\\d"));
	}) ? "yes" : "no") << std::endl;
	check("List Length", [&] {
		return words.size() == (mode8k ? std::pow(2, 13) : std::pow(6, 5));
	});
	if (!mode8k) {
		check("Correct dice rolls", [&] {
			for (int i = 0; i < rolls.size(); ++i) {
				auto roll = rolls[i];
				auto p = 1;
				auto acc = 0;
				for (const auto& c: roll | reversed) {
					acc += p*(c - '1');
					p *= 6;
				}
				if (acc != i) { return false; }
			}
			return true;
		});
	}
	check("No duplicate words", [&] {
		auto r = adjacent_find(words);
		if (r != words.end()) {
			std::cerr << "'" << *r << "' is duplicate. ";
			return false;
		}
		else {
			return true;
		}
	});
	check("No composite words", [&] {
		std::unordered_set<std::string> allWords(words.begin(), words.end());
		std::vector<std::string> wordsByLength(words.begin(), words.end());
		sort(wordsByLength, [](const auto& a, const auto& b) {
			return a.size() > b.size();
		});
		// int i = 0;
		return wordsByLength.end() == find_if(wordsByLength, [&](const auto& word) {
			// std::cerr << " " << (int) (100*i++ / (double) words.size()) << "%\rNo composite words: " << std::flush;
			return isComposite(word, wordsByLength, allWords, minMaxLength.second);
		});
	});
	
	return errors ? -1 : 0;
}
