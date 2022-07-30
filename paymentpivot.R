library(bs4Dash)
library(DT)
library(pool)
library(dplyr)
library(tidyverse)
library(DBI)
library(RPostgres)
library(ggcharts)
library(ggplot2)
library(echarts4r)
library(janitor)
library(lubridate)
library(feather)
library(fst)
library(shinyvalidate)
library(shinyjs)
library(sodium)
library(httr)
library(bslib)
library(fresh)
library(splitstackshape)
library(zoo)
library(data.table)
library(flextable)
library(glue)
library(shinyFiles)
library(collapse)
library(shinyFeedback)
library(memoise)
library(readr)
library(readxl)
library(shinyalert)
library(lares)
library(uuid)
library(fontawesome)
library(shinyBS)
library(shinyWidgets)

forex_e_rate = 63.20

shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache")))

myToastOptions <- list(
  positionClass = "toast-top-right",
  progressBar = FALSE,
  timeOut = 2000,
  closeButton = TRUE,
  # same as defaults
  newestOnTop = TRUE,
  preventDuplicates = FALSE,
  showDuration = 300,
  hideDuration = 1000,
  extendedTimeOut = 1000,
  showEasing = "linear",
  hideEasing = "linear",
  showMethod = "fadeIn",
  hideMethod = "fadeOut")


bs4dash_font(size_base = "1.5rem", weight_bold = 900)
thematic::thematic_shiny(font = "auto")
options(scipen = 9999)
options(digits=15)
options(warn = 0)
e_rate = 64.46
forex_e_rate = 63.36


title <- tags$a(href='https://www.google.com',
                tags$img(src="PROCAVA_LOGO.png", height = '92.5', width = '220'),
                '', target="_blank")

db <- 'mozprocava'  
host_db <- "procavamoz.cqjbzdmqzoph.us-east-1.rds.amazonaws.com"
db_port <- '5432'  
db_user <- "postgres"
db_password <- "MZprocava;2030"
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIATXOOGZ5WJ73YXSNB", "AWS_SECRET_ACCESS_KEY" = "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", "AWS_DEFAULT_REGION" = "us-east-1")
pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

# onStop(function() {poolClose(pool)})

# approval_requests <- 
approval_requests <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.approval_requests") %>% pivot_longer(cols = c('ifadloan_pct', 'ifadgrant_pct', 'ifadrpsf1_pct', 'ifadrpsf2_pct',
                                                                 'beneficiaryinkind_pct', 'beneficiarymonetary_pct',
                                                                 'privateinkind_pct', 'privatemoney_pct', 'governmentinkind_pct',
                                                                 'governmentmoney_pct'), names_to = "financier", values_to = "percent_financed") %>% fsubset(percent_financed > 0) %>% fsubset(usd_paid>0)
approval_requests$usd_paid <- approval_requests$usd_paid*approval_requests$percent_financed

library(readxl)
correct_events <- read_excel("C:/Users/Administrator/Desktop/correct_events.xlsx")
dbWriteTable(pool, SQL("value_chains.eventos"), correct_events, overwrite = FALSE, append = TRUE)
