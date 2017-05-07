library(magrittr)
library(data.table)
library(stringr)
library(RSQLite)
library(PTTr)
library(assertthat)
source("data_stream.R")

# try -----------------------------------------------------------------------

# Crawler
# dt <- get_all_posts("GIVE", max_post = 20)

# Save to SQLite
con <- dbConnect(RSQLite::SQLite(), "pttdb.sqlite")

# dbDisconnect(con)


# Update data -------------------------------------------------------------

# update_ptt_data(con, "GIVE", n_new_post = 1000)

# Load data ---------------------------------------------------------------

dt <- load_ptt_give()
library(ggplot2)

dt[post_time > as.POSIXct(Sys.Date() - 10)] %>% 
  ggplot(aes(x = post_time)) +
  geom_histogram(color = "white", bins = 30) +
  labs(title = "Number of posts") +
  theme_light()



