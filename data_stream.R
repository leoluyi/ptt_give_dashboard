library(magrittr)
library(data.table)
library(assertthat)
library(stringr)
library(RSQLite)
library(PTTr) # Get PTT data. devtools::install_github("leoluyi/PTTr")
library(shiny)
library(shinySignals) # devtools::install_github("hadley/shinySignals")
library(bubbles) # devtools::install_github("jcheng5/bubbles")


data_stream <- function(session = getDefaultReactiveDomain()) {
  dt_reac <- reactive({
    autoInvalidate(session)
    dt <- load_ptt_give()
    dt[, post_time := strftime(post_time, format="%Y-%m-%d %H:%M:%S")]
    
    randomly <- function(x) sample(xtfrm(x))
    dt #%>% arrange(randomly(post_id))
  })
}

# Utils -------------------------------------------------------------------

update_ptt_data <- function(con, board, n_new_post = 20, parallel = TRUE) {
  # board = "give"
  # n_new_post = 20
  # parallel = TRUE

  assertthat::assert_that(DBI::dbIsValid(con))
  
  if (n_new_post <= 20) {
    parallel <- FALSE
  }
  
  posts <- get_posts_list(board, max_post = n_new_post, parallel=parallel)
  old_post_id <- dbGetQuery(con, "select post_id from PTT") %>% unlist(use.names = F)
  new_post_id <- setdiff(posts$post_id, old_post_id)
  
  if (length(new_post_id) == 0) {
    message("No new posts")
    return(invisible())
  }
  if (length(new_post_id) <= 20) {
    parallel <- FALSE
  }
  
  new_post_urls <- posts[posts$post_id %in% new_post_id, "post_urls"] %>% unlist(use.names=F)
  new_dt <- get_all_posts_from_url(new_post_urls, max_post = n_new_post, 
                                   parallel = parallel)
  # old_record <- dbGetQuery(con, "SELECT post_id FROM PTT") %>% .[[1]]
  # to_write <- new_dt[!post_id %in% old_record]
  
  success_list <- vector(length = nrow(new_dt))
  n_old_post <- 0
  for (row in seq(nrow(new_dt))) {
    tryCatch({
      success <- dbWriteTable(con, "PTT", new_dt[row,], append = TRUE)
      success_list[row] <- success
    },
    error = function(e) {
      if (grepl("UNIQUE constraint failed", e$message)) {
        n_old_post <<- n_old_post + 1
      } else {
        message(e)
      }
    })
  }
  message(n_old_post, " old post(s)")
  message(sum(success_list), " post(s) updated")
  invisible(new_dt)
}


load_data <- function(con, table) {
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  setDT(data)
  data[, post_time := .(as.POSIXct(post_time, origin = "1970-01-01 00:00.00 UTC"))]
  data
}

load_ptt_give <- function() {
  con = dbConnect(RSQLite::SQLite(), "pttdb.sqlite")
  dt <- load_data(con, "PTT") %>% setDT
  dt <- dt[title %>% str_detect("贈送")]
  dt <- dt[order(post_time, decreasing = T)]
  
  dt[, item_detail := post_text %>% 
       str_match("(?s)物\\s{0,4}[品況][:：](.*?)(?:到期日|領取地點|期\\s*限|$)") %>% 
       .[,2] %>% 
       str_trim() %>% 
       str_replace_all("\n+", " ")]
  dt[, expiration_date := post_text %>% 
       str_match("(?s)到期日.*?[:：](.*?)(?:領取|地點|期\\s*限|$)") %>%
       .[,2] %>%
       str_trim() %>% 
       str_replace_all("\n", " ")]
  dt[, location_detail := post_text %>% 
       str_match("(?s)(?:領取)?地點[:：](.*?)(?:期\\s*限|聯絡|$)") %>% 
       .[,2] %>% str_replace_all("\n", " ") %>% str_trim()]
  dt[, before := post_text %>% 
       str_match("(?m)期\\s{0,4}限[:：](.*)$") %>% .[,2]]
  dt[, contact := post_text %>% 
       str_match("(?m)\\s聯絡方[法式][:：](.*)$") %>% .[,2]]
  dt[, remarks := post_text %>% 
       str_match("(?s)備\\s{0,4}註[:：](.+?)(?:--.+)?$") %>% .[,2] %>% 
       str_trim() %>% str_replace_all("\n+", " ")]
  dt[, location := title %>% 
       str_match("\\[贈送\\]\\s?(.+?)[^\\w]") %>%
       .[,2] %>% str_replace_all("[^\\w]+", "")]
  dt[, item := title %>% 
       str_match("([^-_\\s]*)$") %>%
       .[,2] %>% str_replace_all("[^\\w]", "")][
         ,item := ifelse(is.na(item),
                         title %>% 
                           str_match("\\s(.*)$") %>%
                           .[,2] %>% str_replace_all("[^\\w]", ""),
                         item)]
  dt
}
