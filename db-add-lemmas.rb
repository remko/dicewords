#!/usr/bin/env ruby

require 'sqlite3'
require 'unidecoder'
require 'csv'

def skipLine?(line)
  line.start_with?("#") or line.empty?
end

db = SQLite3::Database.new "words.db"

score_columns = db.execute("SELECT name from pragma_table_info('candidate_words')").map{ |c| c[0] }[1..-1]

db.execute("DROP VIEW IF EXISTS candidate_lemmas")
sum_scores_sql = score_columns.map {|c| "SUM(candidate_words.#{c}) AS #{c}" }.join(", ")
max_scores_sql = score_columns.map {|c| "max(#{c}) AS #{c}_max" }.join(", ")
scaled_scores_sql = score_columns.map {|c| "#{c} / (SELECT #{c}_max FROM minmax) AS #{c}"}.join(", ")
db.execute(<<-SQL)
  CREATE VIEW candidate_lemmas AS
    WITH 
      lemmas_scores AS (
        SELECT 
          subtlex_lemmas.id AS lemma_id,
          subtlex_lemmas.word AS lemma,
          subtlex_lemmas.type AS type,
          #{sum_scores_sql}
        FROM subtlex_lemma_forms
        INNER JOIN subtlex_lemmas
        ON subtlex_lemmas.id = subtlex_lemma_forms.lemma
        INNER JOIN candidate_words
        ON candidate_words.word = subtlex_lemma_forms.form
        GROUP BY lemma_id, lemma, type
      ),
      minmax AS (SELECT #{max_scores_sql} FROM lemmas_scores)
    SELECT 
      lemma, type, 
      #{scaled_scores_sql}
    FROM lemmas_scores
SQL


exit 0

all_valid_words = Set.new(db.execute("select word from `all_valid_words`").map { |r| r[0] })

db.transaction

db.execute(<<-SQL)
  CREATE TABLE IF NOT EXISTS `subtlex_lemmas` (
    `id` INTEGER NOT NULL,
    `word` TEXT NOT NULL, 
    `type` TEXT NOT NULL,
    PRIMARY KEY(`id`),
    UNIQUE (word, type)
  )
SQL
db.execute("DELETE FROM `subtlex_lemmas`")
db.execute(<<-SQL)
  CREATE TABLE IF NOT EXISTS `subtlex_lemma_forms` (
    `form` TEXT NOT NULL, 
    `lemma` INTEGER,
    FOREIGN KEY(lemma) REFERENCES subtlex_lemmas(id),
    UNIQUE (form, lemma)
  )
SQL
db.execute("DELETE FROM `subtlex_lemma_forms`")

current_lemma = nil
current_lemma_id = 0
current_lemma_valid = false
CSV.foreach("words/nl/subtlex-lemmas.csv", col_sep: "\t", quote_char: "\0") do |row|
  next if row[0].start_with?("#")
  if row[0] == "@"
    db.execute("INSERT OR IGNORE INTO subtlex_lemma_forms(form, lemma) VALUES(?, ?)", row[2], current_lemma_id) if current_lemma_valid
  else
    current_lemma = row[0]
    type = row[1]
    current_lemma_valid = all_valid_words.include?(current_lemma)
    if current_lemma_valid
      begin
        lemma_id = current_lemma_id + 1
        db.execute("INSERT INTO subtlex_lemmas(id, word, type) VALUES(?, ?, ?)", lemma_id, current_lemma, type)
        current_lemma_id = lemma_id
      rescue SQLite3::ConstraintException
        # puts "Ignoring duplicate #{current_lemma} #{type}"
        current_lemma_valid = false
      end
    end
  end
end
db.commit
