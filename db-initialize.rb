#!/usr/bin/env ruby

require 'sqlite3'
require 'unidecoder'
require 'csv'

MINIMUM_SCORE = 0.01 # Occurring in a score list counts for something
MAXIMUM_WORD_LENGTH = 6

def normalizeWord(word)
  word = word.to_ascii.downcase.gsub(/\./, '') # Abbreviations are OK
  if not word.match(/^[a-zA-Z]+$/)
    # puts "Skipping '#{word}'"
    return ""
  end
  word
end

def skipLine?(line)
  line.start_with?("#") or line.empty?
end

db = SQLite3::Database.new "words.db"
db.transaction

# Iterate over all score files
score_files = [
  ["words.txt", "words", false, false, true, :word],
  ["score.leipzig.txt", "leipzig_score", true, true, true, :word],
  ["score.leipzig-corpora.txt", "leipzig_corpora_score", true, true, true, :score_word],
  ["score.opensubtitles.txt", "opensubtitles_score", true, true, true, :word_score],
  ["score.sprookjes.txt", "sprookjes_score", true, true, true, :word_score],
  ["crr-prevalence.csv", "crr_prevalence_score", true, false, true, :crr_prevalence],
  ["voornamen.csv", "voornamen_score", true, false, false, :csv],
]
score_files.each do |f, name, includeScore, applyLog, normalize, format|
  puts "Importing #{name}"

  lines = File.read("words/nl/#{f}").lines
  total = lines.length

  if includeScore
    db.execute(<<-SQL)
      CREATE TABLE IF NOT EXISTS `#{name}` (
        `word` TEXT NOT NULL UNIQUE, 
        `score` INTEGER NOT NULL, 
        PRIMARY KEY(`word`)
      )
    SQL
    db.execute("DELETE FROM `#{name}`")
    frequencies = Hash.new(0)
    lines.each_with_index do |line, i|
      line = line.strip
      next if skipLine?(line)
      if format == :word
        word = line
        score = total - i
      elsif format == :score_word
        score, _, word = line.partition(" ")
      elsif format == :word_score
        word, _, score = line.rpartition(" ")
      elsif format == :crr_prevalence
        l = CSV.parse_line(line, col_sep: "\t")
        word = l[0]
        score = l[2]
      elsif format == :csv
        l = CSV.parse_line(line, col_sep: ",")
        word = l[0].downcase
        score = l[1]
      end
      word = normalizeWord(word) if normalize
      score = score.to_f
      next if word.empty?
      frequencies[word] = frequencies[word] + score
    end
    frequencies.each { |k,v| frequencies[k] = 1 + Math.log10(v) } if applyLog
    min, max = frequencies.values.minmax
    frequencies.each do |word, score| 
      score = MINIMUM_SCORE + (1.0 - MINIMUM_SCORE) * (score - min) / (max - min)
      db.execute("INSERT INTO `#{name}`(word, score) VALUES(?, ?)", word, score)
    end
  else
    db.execute(<<-SQL)
      CREATE TABLE IF NOT EXISTS `#{name}` (
        `word` TEXT NOT NULL UNIQUE, 
        PRIMARY KEY(`word`)
      )
    SQL
    db.execute("DELETE FROM `#{name}`")
    lines.each do |line|
      word = line.strip
      next if skipLine?(word)
      word = normalizeWord(word)
      next if word.empty?
      db.execute("INSERT OR IGNORE INTO `#{name}`(word) VALUES(?)", word)
    end
  end
end

db.execute(<<-SQL)
  CREATE TABLE IF NOT EXISTS `word_status` (
    `word` TEXT NOT NULL UNIQUE, 
    `vandale_status` INTEGER DEFAULT 0, 
    `woordenlijst_status` INTEGER DEFAULT 0, 
    PRIMARY KEY(`word`)
  )
SQL

# Create views
db.execute("DROP VIEW IF EXISTS all_words")
db.execute(<<-SQL)
  CREATE VIEW `all_words` (`word`) AS 
    SELECT word from words 
    UNION 
    SELECT word from leipzig_score 
    UNION 
    SELECT word from leipzig_corpora_score 
    UNION 
    select word from sprookjes_score
    UNION 
    select word from crr_prevalence_score
SQL
# db.execute("CREATE VIEW `all_words` (`word`) AS SELECT word from words")

db.execute("DROP VIEW IF EXISTS all_valid_words")
db.execute(<<-SQL)
  CREATE VIEW `all_valid_words` (`word`) AS 
    SELECT all_words.word from all_words 
    INNER JOIN word_status ON word_status.word = all_words.word 
    WHERE word_status.vandale_status = 1
SQL

db.execute("DROP VIEW IF EXISTS `length_score`")
db.execute(<<-SQL)
  CREATE VIEW `length_score` AS 
    WITH minmax AS (
      SELECT 
        min(length(word)) as min, 
        max(length(word)) as max 
      FROM all_valid_words
    ) 
    SELECT 
      word, 
      #{MINIMUM_SCORE} + (1.0 - #{MINIMUM_SCORE}) * (1.0 - 
        (1.0*(length(word) - (SELECT min from minmax)) / 
          ((SELECT max from minmax) - (SELECT min from minmax)))) 
        AS score 
    FROM all_valid_words
SQL

db.execute("DROP VIEW IF EXISTS candidate_words")
score_columns = ["length_score"] + score_files.select { |x| x[2] }.map{|x| x[1] }
score_column_definitions = score_columns.map { |c| "IFNULL(#{c}.score, 0) AS #{c}" }.join(", ")
score_join_definitions = score_columns.map { |c| "LEFT JOIN #{c} ON #{c}.word = all_valid_words.word"}.join(" ")
db.execute(<<-SQL)
  CREATE VIEW `candidate_words` AS 
    SELECT 
      all_valid_words.word, 
      #{score_column_definitions} 
    FROM all_valid_words 
    #{score_join_definitions} 
    WHERE length(all_valid_words.word) <= #{MAXIMUM_WORD_LENGTH}
SQL

score_columns.each do |name|
    db.execute("DROP VIEW IF EXISTS #{name}_histogram")
    db.execute(<<-SQL)
      CREATE VIEW `#{name}_histogram` AS
        SELECT 
          min(CAST((score*10) as int)/10.0, 0.9) AS score,
          COUNT(*) as count
        FROM #{name}
        GROUP BY 1
    SQL
end

db.commit
