library(tidyverse)
library(rvest)
library(rlist)
library(XML)
library(httr)

test <- read_html("https://www.pro-football-reference.com/years/2018/draft.htm")

draft <- test %>%
  html_nodes("#drafts") %>%
  html_text()

print(draft)

urls <- test %>%
  html_nodes("a") %>%
  html_attr("href")

test_url <- urls[15]
  parse_url(test_url)
