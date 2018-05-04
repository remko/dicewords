#!/usr/bin/env ruby

require 'httpclient'
require 'json'
require 'sqlite3'
require 'uri'

$client = HTTPClient.new
# $client.debug_dev = $stdout

def query_vandale(word)
	ok = nil
	while ok.nil?
		begin
			# content = $client.get_content("http://www.vandale.nl/opzoeken", { "lang" => "nn", "pattern" => word, }, { })
			content = $client.get_content("https://www.vandale.nl/gratis-woordenboek/nederlands/betekenis/#{URI::encode(word)}")
			ok = content.include?("googleoff")
		rescue HTTPClient::ConnectTimeoutError
			$stderr.print("Timeout. Retrying")
		end
	end
	ok ? 1 : 2
end

def query_woordenlijst(word)
	ok = nil
	while ok.nil?
		begin
			r = $client.get("http://woordenlijst.org/api-proxy/", {
				"m" => "search",
				"searchValue" => word,
			}, { 
				'Referer' => 'http://woordenlijst.org/' 
			})
			r.status == 200 or raise r.reason
			result = JSON.parse(r.content)
			ok = !result["_embedded"]["entries"].empty?
		rescue HTTPClient::ConnectTimeoutError
			$stderr.print("Timeout. Retrying")
		end
	end
	ok ? 1 : 2
end

db = SQLite3::Database.new "words.db"

db.execute( "select all_words.word from all_words LEFT JOIN word_status ON word_status.word = all_words.word WHERE IFNULL(vandale_status, 0) = 0" ) do |row|
	puts "vandale: Querying '#{row[0]}'"
  db.execute("INSERT OR IGNORE into word_status(word) VALUES (?)", row[0])
	db.execute("UPDATE word_status SET vandale_status = ? WHERE word = ?", query_vandale(row[0]), row[0])
end
db.execute( "select all_words.word from all_words LEFT JOIN word_status ON word_status.word = all_words.word WHERE IFNULL(woordenlijst_status, 0) = 0" ) do |row|
	puts "woordenlijst: Querying '#{row[0]}'"
  db.execute("INSERT OR IGNORE into word_status(word) VALUES (?)", row[0])
	db.execute("UPDATE word_status SET woordenlijst_status = ? WHERE word = ?", query_woordenlijst(row[0]), row[0])
end
puts "Done"
