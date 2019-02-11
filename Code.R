#!/usr/bin/Rscript

library(tidyverse)
library(rvest)
library(XML)
library(RSelenium)
library(magrittr)

library(here)
setwd("~/Projects/Misc/AmesRealEstate/")

source("basic-functions.R")
# Pictures
# Description
# Extra information (if accurate)
# Location

# Zillow filters:
# Within 20 miles of Ames
# .5 acre lot
# 3 bedrooms or greater
# has garage
# Query: https://www.zillow.com/homes/for_sale/fsba,fsbo_lt/house_type/3-_beds/21780-_lot/garage_att/globalrelevanceex_sort/42.400643,-93.288345,41.705216,-94.518814_rect/9_zm/ef51a5aedfX1-CR1n2kaelgv03ge_12pfr1_crid/0_mmm/


url <- "https://www.zillow.com/homes/for_sale/fsba,fsbo_lt/house_type/ef51a5aedfX1-CR1n2kaelgv03ge_12pfr1_crid/3-_beds/21780-_lot/0_singlestory/42.370212,-93.457261,41.735966,-94.3499_rect/9_zm/1_p/0_mmm/"
# Client option
# rd <- remoteDriver(port = 4445L)
# system('docker run -d -p 4445:4444 selenium/standalone-firefox')
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
remDr$open()
# remDr <- rD[["client"]]

# Docker option # It recognizes Docker's browser as a robot
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
# remDr$open()
remDr$navigate(url)

# Establish a wait for an element
# remDr$setImplicitWaitTimeout(1000)
Sys.sleep(4)

pgsrc <- remDr$getPageSource() %>% magrittr::extract2(1) %>% read_html()
articles <-  html_nodes(pgsrc, "article")


# Close driver
remDr$close()
rm(remDr)


house_info <- articles %>% 
  html_attrs() %>%
  lapply(., function(x) as.data.frame(t(x))) %>%
  bind_rows() %>%
  set_names(make.names(names(.)) %>%
              str_replace("data\\.", "")) %>%
  select(photocount, id = zpid, latitude, longitude, sale_type = sgapt) %>%
  mutate_at(.vars = vars(c("latitude", "longitude")), .funs = function(x) as.numeric(x)/1000000)
rm(articles)

item_info <- html_nodes(pgsrc, "article .zsg-photo-card-content") %>%
  map_df(parse_iteminfo)

houses <- full_join(item_info, house_info)
rm(house_info, item_info)

houses_fullinfo <- map(houses$link, get_houseinfo) 

house_photos <- map_df(houses_fullinfo, ~magrittr::extract2(.x, 2))
house_info <- map_df(houses_fullinfo, ~magrittr::extract2(.x, 1)) %>% 
  # Make for easy csv export
  mutate_if(is.character, str_replace_all, pattern = ",", replacement = ";") %>%
  mutate_if(is.character, str_trim) %>%
  mutate_if(is.character, str_replace, pattern = " ?[[:punct:]]$", replacement = "")

house_allinfo <- full_join(houses, house_info)
# rm(houses_fullinfo)

# Only download new photos
new_photos <- filter(house_photos, !file.exists(filename))
map2(new_photos$link, new_photos$filename, ~try(download.file(.x, .y)))
rm(new_photos)

house_photos$saved <- file.exists(house_photos$filename)

# save.image()

if (!exists("house_info.csv")) {
  write_csv(house_info, "house_info.csv")
  write_csv(house_photos, "house_photos.csv")
} else {
  other_info <- read_csv("house_info.csv") %>%
    mutate_all(as.character) %>%
    mutate_all(str_trim)
  tmp <- bind_rows(other_info, house_info) %>%
    mutate(completeness = rowSums(!is.na(.))) %>%
    arrange(id, desc(completeness)) %>%
    group_by(id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(-completeness)
  
  write_csv(tmp, "house_info.csv", append = F)
  rm(tmp)
  
  other_photos <- read_csv("house_photos.csv") %>%
    mutate(id = as.character(id))

  full_join(other_photos, house_photos) %>%
    mutate(completeness = rowSums(!is.na(.))) %>%
    arrange(id, photo_id, desc(completeness)) %>%
    group_by(id, photo_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(-completeness) %>%
    write_csv("house_photos.csv", append = F)
}

system("git add house_info.csv house_photos.csv photos/*")
system("git commit -a -m 'Automatic Update'")
system("git push")
