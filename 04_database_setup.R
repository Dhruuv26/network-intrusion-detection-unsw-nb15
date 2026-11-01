library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

train_processed <- read_csv("unsw_train_processed.csv", show_col_types = FALSE)
test_processed  <- read_csv("unsw_test_processed.csv", show_col_types = FALSE)

cat("Train rows:", nrow(train_processed), " | Test rows:", nrow(test_processed), "\n")

con <- dbConnect(RSQLite::SQLite(), "unsw_project.db")

dbWriteTable(con, "train_data", train_processed, overwrite = TRUE)
dbWriteTable(con, "test_data",  test_processed,  overwrite = TRUE)

cat("Tables in database:", paste(dbListTables(con), collapse = ", "), "\n")

train_from_db <- dbReadTable(con, "train_data")
cat("Rows retrieved from train_data table:", nrow(train_from_db), "\n")

worms_query <- dbGetQuery(con, "SELECT * FROM train_data WHERE attack_cat = 'Worms'")
cat("Worms rows retrieved via SQL WHERE clause:", nrow(worms_query), "\n")

class_counts <- dbGetQuery(con, "
  SELECT attack_cat, COUNT(*) AS n
  FROM train_data
  GROUP BY attack_cat
  ORDER BY n DESC
")
print(class_counts)

dbDisconnect(con)

cat("Database setup complete: unsw_project.db\n")