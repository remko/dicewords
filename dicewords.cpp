#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <boost/range/adaptor/filtered.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/adaptor/indexed.hpp>
#include <boost/range/adaptor/sliced.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/uniqued.hpp>
#include <boost/range/algorithm_ext/insert.hpp>
#include <boost/range/algorithm_ext/erase.hpp>
#include <boost/range/algorithm_ext/push_back.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/algorithm/max_element.hpp>
#include <boost/range/numeric.hpp>
#include <boost/regex.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
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

auto WORD_REGEX = boost::regex("^[a-zA-Z]+$");
auto LOWER_WORD_REGEX = boost::regex("^[a-z]+$");
auto MAXIMUM_WORD_SIZE = 6;
auto MAXIMUM_COMPONENT_FREQUENCY = 20;
auto SIZE_WEIGHT = 0.0;
auto ROLLS = 5;
auto MINIMUM_SCORE = 0.01; // Occurring in a score list counts for something

std::string stringToLower(std::string word) {
	transform(word, word.begin(), ::tolower);
	return word;
}

// Remove '.' and '-' combining characters
// std::string removeCombiningCharacters(std::string word) {
// 	word.erase(remove_if(word, is_any_of("-.")), word.end());
// 	return word;
// }

bool isASCIIWord(const std::string& word) {
	return regex_search(word.begin(), word.end(), WORD_REGEX);
}

bool isLowerASCIIWord(const std::string& word) {
	return regex_search(word.begin(), word.end(), LOWER_WORD_REGEX);
}

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

std::vector<std::string> readWords(const std::string& filename) {
	std::vector<std::string> words;
	std::ifstream infile(filename);
	std::string line;
	while (std::getline(infile, line)) {
		std::istringstream iss(line);
		std::istream_iterator<std::string> it(iss);
		if (it != std::istream_iterator<std::string>()) {
			if ((*it)[0] != '#') {
				words.push_back(*it);
			}
		}
	}
	return words;
}

// Read words from file & preprocess them
std::vector<std::string> preprocessWords(std::vector<std::string> input) {
	std::vector<std::string> words;
	push_back(words, 
		input
		// | transformed(removeCombiningCharacters)
		| filtered(isASCIIWord)
		| filtered(isLowerASCIIWord)
		| transformed(stringToLower)
		| filtered([](const auto & word) {
			return word.size() > 1 && word.size() <= MAXIMUM_WORD_SIZE;
		})
	);

	std::vector<std::string> result;
	push_back(result, boost::unique(sort(words)));
	return result;
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

void writeHistogram(const std::unordered_map<std::string, double>& scores, const std::string& file) {
	auto histogram = std::vector<int>(10, 0);
	for (const auto& score : scores) {
		histogram[std::min((int) std::floor(score.second*10), 9)]++;
	}
	std::ofstream out(file + ".hist.csv");
	for (int i = 0; i < histogram.size(); ++i) {
		out << i/10.0 << "-" << (i+1)/10.0 << "," << histogram[i] << std::endl;
	}
}

std::unordered_map<std::string, double> parseScores(const std::string& file, bool withFrequencies) {
	std::unordered_map<std::string, double> result;
	std::ifstream infile(file);
	std::string line;
	if (withFrequencies) {
		std::vector<std::string> words;
		while (std::getline(infile, line)) {
			trim(line);
			if (starts_with(line, "#") || line.empty()) { continue; }
			auto spaceIndex = line.rfind(' ');
			assert(spaceIndex != std::string::npos);
			auto word = line.substr(0, spaceIndex);
			if (!isASCIIWord(word)) { continue; }
			auto frequency = std::stod(line.substr(spaceIndex + 1));
			assert(frequency > 0);
			result[stringToLower(word)] = 1 + std::log10(frequency);
		}
	}
	else {
		std::vector<std::string> words;
		while (std::getline(infile, line)) {
			trim(line);
			if (starts_with(line, "#") || line.empty()) { continue; }
			auto word = line;
			if (!isASCIIWord(word)) { continue; }
			words.push_back(stringToLower(word));
		}
		int score = words.size() + 1;
		for (const auto& word : words) {
			result[word] = score;
			score--;
		}
	}

	// Normalize score to 0-1 interval
	auto minmax = std::minmax_element(result.begin(), result.end(), [](const auto& p1, const auto& p2) {
		return p1.second < p2.second;
	});
	auto min = minmax.first->second;
	auto max = minmax.second->second;
	for (auto& kv : result) {
		kv.second = MINIMUM_SCORE + (1.0 - MINIMUM_SCORE) * (kv.second - min) / (max - min);
	}
	// writeHistogram(result, file);
	return result;
}

struct ScoreFile {
	std::string file;
	double weight;
	bool withFrequencies;
};

struct Scores {
	std::unordered_map<std::string, double> wordScores;
	double weight;
};

std::vector<std::pair<std::string, double>> scoreWords(
		const std::vector<std::string>& words, 
		const std::vector<ScoreFile>& scoreFiles) {

	std::vector<Scores> scores;
	push_back(scores, scoreFiles | transformed([](const auto& s) {
			return Scores { parseScores(s.file, s.withFrequencies), s.weight };
	}));

	std::vector<std::pair<std::string, double>> result;
	push_back(result, words | transformed([&](const std::string& word) {
		auto score = accumulate(scores, 0.0, [&](double acc, const Scores& scores) {
			auto i = scores.wordScores.find(word);
			return i != scores.wordScores.end() ? acc + scores.weight * i->second : acc;
		});

		score += SIZE_WEIGHT * (MAXIMUM_WORD_SIZE - word.size() / (MAXIMUM_WORD_SIZE - 1));
		return std::make_pair(word, score);
	}));
	return result;
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
	auto candidateFile = argv[3];
	std::vector<ScoreFile> scoreFiles;
	for (int i = 4; i < argc; i += 3) {
		scoreFiles.push_back(ScoreFile {
			argv[i],
			std::stod(argv[i+1]),
			std::string(argv[i+2]) == "frequencies"
		});
	}

	std::cerr << "Loading candidate words ... " << std::flush;
	auto candidateWords = readWords(candidateFile);
	std::cerr << candidateWords.size() << " words remaining" << std::endl;

	std::cerr << "Preprocessing candidate words ... " << std::flush;
	auto words = preprocessWords(readWords(candidateFile));
	std::cerr << words.size() << " words remaining" << std::endl;

	std::vector<std::string> simpleWords;
	if (!composites) {
		std::cerr << "Removing composite words ... " << std::flush;
		simpleWords = removeCompositeWords(words);
		std::cerr << simpleWords.size() << " words remaining" << std::endl;
	}
	else {
		simpleWords = words;
	}

	std::cerr << "Scoring words ..." << std::endl;
	auto scoredWords = scoreWords(simpleWords, scoreFiles);

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
