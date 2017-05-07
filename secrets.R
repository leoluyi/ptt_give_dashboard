# secrets.R
options(
  mysql = list(
    "host" = "YOUR SQL IP",
    "port" = 3306,
    "user" = "YOUR SQL USER",
    "password" = "YOUR USER PW",
    "databaseName" = "onlinegashiny"),
  rga = list(
    "profile_id" = "The GA View ID",
    "daysBackToFetch" = 356*3
  ),
  shinyMulti = list(
    "max_plots" = 10
  ),
  myCausalImpact = list(
    "test_time" = 14,
    "season" = 7
  ),
  shiny.maxRequestSize = 0.5*1024^2 ## upload only 0.5 MB
)
