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

list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  
  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))
  
  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }
  
  df
}

library(tidyverse)
library(rvest)
library(XML)
library(RSelenium) # start a server with utility function
library(magrittr)

# Open selenium driver in docker
# system('docker run -d -p 4445:4444 selenium/standalone-chrome')

parse_iteminfo <- function(y) {
  x <- html_children(y)
  address <- html_children(x[1]) %>% html_text()
  location <- html_children(x[2]) %>% html_attr("content") %>% as.numeric()
  info <- html_children(x[3]) %>% html_children() 
  info2 <- html_text(info[3], trim = T) %>% str_split("·") %>% unlist() %>%
    str_trim() %>%
    str_split(" ") %>%
    lapply(function(z) data_frame(zz = z[1]) %>% set_names(str_trim(z[2]))) %>%
    bind_cols() %>%
    mutate_all(parse_number)
  
  df <- data_frame(street = address[1],
                   city = address[2],
                   state = address[3], 
                   zip = address[4],
                   lat = location[1], lon = location[2],
                   price = parse_number(str_replace(html_text(info[2]), "K", "000")),
                   link = html_attr(x[4], "href"),
                   id = str_extract(link, "\\d{1,}_zpid") %>% str_replace("_zpid", "")
                   ) %>%
    mutate_if(is.character, str_trim) %>%
    bind_cols(info2)
  
  df
}

get_houseinfo <- function(h_url, photo_dir = "photos") {
  if (!dir.exists(photo_dir)) {
    dir.create(photo_dir)
  }
  
  id <- str_extract(h_url, "\\d{1,}_zpid") %>% str_replace("_zpid", "")
  
  if (!str_detect("http", h_url)) {
    h_url <- paste0("http://zillow.com", h_url, "/?fullpage=TRUE")
  }
  
  src <- read_html(h_url)
  desc <- html_nodes(src, ".hdp-header-description .zsg-content-item") %>% html_text()
  
  facts_glance <- html_nodes(src, ".zsg-media-bd .hdp-fact-ataglance-value") %>% 
    html_text(trim = T) %>% t() %>% as_data_frame() %>%
    set_names(html_nodes(src, ".zsg-media-bd .hdp-fact-ataglance-heading") %>% 
                html_text(trim = T))
  facts_full <- html_nodes(src, ".hdp-fact-container-columns .hdp-fact-list li") %>% html_text()
  
  facts_named <- facts_full[str_detect(facts_full, ":")]
  facts_unnamed <- facts_full[!str_detect(facts_full, ":")]
  facts_unnamed_basement <- facts_unnamed[str_detect(facts_unnamed, "basement")] %>%
    str_replace(" ?basement", "") %>%
    paste(collapse = ", ") %>%
    paste("basement:", .)
  facts_named <- c(facts_named, facts_unnamed_basement)
  facts_unnamed <- facts_unnamed[!str_detect(facts_unnamed, "basement")]
  
  # Split facts named into name/value pairs
  facts_named_df <- tidyr::extract(data_frame(x = facts_named), x, into = c("name", "value"), remove = T, regex = "(.*): ?(.*)") %>%
    mutate(name = str_to_title(name) %>% str_replace(" Included| Material|ing", "")) %>%
    group_by(name) %>%
    summarize(value = value %>% str_split(", ?") %>% unlist %>% unique() %>% paste(collapse = "; ")) %>%
    tidyr::spread(key = name, value = value) %>%
    mutate(id = id)
  
  photos <- html_nodes(src, ".img-wrapper img") %>% html_attrs() %>%
    map_df(.f = function(z) {
      names(z) <- str_replace(names(z), "href|src", "link")
      data_frame(id = id, link = z["link"], photo_id = z["id"], class = z["class"], filename = paste0(photo_dir, "/", str_extract(link, "[A-z0-9]*.[a-z]{3,4}$")))
    })
  
  list(facts = bind_cols(facts_named_df, facts_glance), photos = photos)
  
}

url <- "https://www.zillow.com/homes/for_sale/fsba,fsbo_lt/house_type/ef51a5aedfX1-CR1n2kaelgv03ge_12pfr1_crid/3-_beds/21780-_lot/0_singlestory/42.370212,-93.457261,41.735966,-94.3499_rect/9_zm/1_p/0_mmm/"
# Client option
rD <- rsDriver(browser = "chrome")
remDr <- rD[["client"]]

# Docker option # It recognizes Docker's browser as a robot
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()
remDr$navigate(url)

# Establish a wait for an element
remDr$setImplicitWaitTimeout(1000)

pgsrc <- remDr$getPageSource() %>% magrittr::extract2(1) %>% read_html()
articles <-  html_nodes(pgsrc, "article")


# Close driver initially
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
rm(houses_fullinfo)

# Only download new photos
new_photos <- filter(house_photos, !file.exists(filename))
map2(new_photos$link, new_photos$filename, download.file)
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
