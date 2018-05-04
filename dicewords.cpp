#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <sqlite3.h>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/adaptor/indexed.hpp>
#include <boost/range/adaptor/sliced.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm_ext/push_back.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/numeric.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <algorithm>
#include <memory>
#include <unordered_set>
#include <unordered_map>
#include <functional>
#include <cassert>
#include <cstdlib>

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#endif

using namespace boost::adaptors;
using namespace boost::algorithm;
using namespace boost;
using namespace std::placeholders;

auto MAXIMUM_COMPONENT_FREQUENCY = 20;
auto ROLLS = 5;

std::vector<std::string> decompose(const std::string& word, const std::unordered_set<std::string>& allWords) {
	assert(!word.empty());
	for (int i = 1; i <= word.size(); ++i) {
		std::string candidateWord = word.substr(0, i);
		if (allWords.find(candidateWord) != allWords.end()) {
			if (i == word.size()) {
				return std::vector<std::string>{candidateWord};
			}
			else {
				auto rest = decompose(word.substr(i), allWords);
				if (!rest.empty()) {
					rest.push_back(candidateWord);
					return rest;
				}
			}
		}
	}
	return std::vector<std::string>();
}

std::vector<std::string> removeCompositeWords(std::vector<std::string> words) {
	std::unordered_set<std::string> allWords(words.begin(), words.end());

	// Remove words that occur frequently in other words.
	// This avoids that we throw out too many words.
	while (true) {
		std::unordered_map<std::string, int> counts;
		for (const auto& word : words) {
			for (const auto& component : decompose(word, allWords)) {
				auto i = counts.find(component);
				if (i != counts.end()) {
					i->second++;
				}
				else {
					counts.insert(std::make_pair(component, 1));
				}
			}
		}
		auto max = max_element(counts, [](const auto& p1, const auto& p2) {
			return p1.second < p2.second;
		});
		assert(max != counts.end());
		if (max->second > MAXIMUM_COMPONENT_FREQUENCY) {
			allWords.erase(max->first);
		}
		else {
			break;
		}
	}

	std::vector<std::string> filteredWords(allWords.begin(), allWords.end());
	filteredWords.erase(remove_if(filteredWords, [&](const auto& word) {
		return decompose(word, allWords).size() > 1;
	}), filteredWords.end());

	return filteredWords;
}

std::string toDiceRolls(int i) {
	std::vector<int> digits;
	int acc = i;
	while (acc) {
		auto r = std::div(acc, 6);
		digits.push_back(r.rem);
		acc = r.quot;
	}
	while (digits.size() < ROLLS) {
		digits.push_back(0);
	}

	std::string result;
	push_back(result, digits
			| reversed
			| transformed([](int i) { return '1' + i; }));
	return result;
}

std::string scoredWordToString(const std::pair<std::string, double>& w) {
	return w.first + (false ? " (" + std::to_string(w.second ) + ")" : "");
;
}

#ifndef UNITTESTS

int main(int argc, const char* argv[]) {
	// TODO: Create command-line parameters for these
	// bool debug = false;
	if (argc < 4) { std::cout << "Error" << std::endl; return -1; }
	bool dice = std::string(argv[1]) != "8k";
	bool composites = std::string(argv[2]) == "composites";
	std::vector<int> weights;
	for (int i = 3; i < argc; i++) {
		weights.push_back(std::stod(argv[i]));
	}

	// Read & score words
	std::vector<std::pair<std::string, double>> scoredWords;
	sqlite3* db;
	sqlite3_open("words.db", &db);
	if (db == nullptr) { throw std::runtime_error("error opening DB"); }
	sqlite3_stmt* stmt;
	sqlite3_prepare_v2(db, "SELECT * from candidate_words", -1, &stmt, nullptr);
	while(sqlite3_step(stmt) != SQLITE_DONE) {
		int num_cols = sqlite3_column_count(stmt);
		assert(num_cols == weights.size() + 1);
		std::string word((const char*) sqlite3_column_text(stmt, 0));
		double score = 0;
		for (int i = 1; i < num_cols; ++i) {
			score += score + weights[i-1] * sqlite3_column_double(stmt, i);
		}
		scoredWords.push_back(std::make_pair(word, score));
	}
	sqlite3_finalize(stmt);
	sqlite3_close(db);


	if (!composites) {
		std::cerr << "Removing composite words from " << scoredWords.size() << " words ... " << std::flush;
		std::vector<std::string> words;
		push_back(words, scoredWords | transformed([](const auto& w) { return w.first; }));
		words = removeCompositeWords(words);
		
		std::unordered_set<std::string> simpleWords(words.begin(), words.end());
		scoredWords.erase(remove_if(scoredWords, [&](const auto& w) {
			return simpleWords.find(w.first) == simpleWords.end();
		}), scoredWords.end());

		std::cerr << scoredWords.size() << " words remaining" << std::endl;
	}

	// Sort final results
	sort(scoredWords, [](const auto& p1, const auto& p2) {
		return p1.second > p2.second;
	});

	std::vector<std::string> finalWords;
	push_back(finalWords, scoredWords
			| transformed(scoredWordToString)
			// | transformed([debug](const auto& w) { 
			// 	return w.first + (debug ? " (" + std::to_string(w.second ) + ")" : "");
			// })
			| sliced(0, std::min((unsigned int) scoredWords.size(), dice ? (unsigned int) pow(6, ROLLS) : 8192U)));
	sort(finalWords);

	auto dicewords = finalWords | indexed(0);
	std::cout << "# Source: https://el-tramo.be/blog/diceware-nl" << std::endl;
	std::cout << std::endl;
	for (auto i = boost::begin(dicewords); i < boost::end(dicewords); ++i) {
		if (dice) {
			std::cout << toDiceRolls(i->index()) << "\t" << i->value() << std::endl;
		}
		else {
			std::cout << i->value() << std::endl;
		}
	}

	auto averageWordLength = 
		accumulate(finalWords, 0, [](auto acc, const auto& s) { return acc + s.size(); }) 
		/ (double) finalWords.size();
	std::cerr << dicewords.size() << " dicewords written" << std::endl;
	std::cerr << "Average word length: " << averageWordLength << " characters" << std::endl;

	return 0;
}

#else

TEST_CASE("removeCompositeWords") {
	SECTION( "works with composite words" ) {
		std::vector<std::string> words = { "ijs", "kap", "ijskap" };
		auto result = removeCompositeWords(words);
		sort(result);
		REQUIRE(result.size() == 2);
		REQUIRE(result[0] == "ijs");
		REQUIRE(result[1] == "kap");
	}
}

#endif
