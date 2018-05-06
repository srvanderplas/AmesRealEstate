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
  
  if (!str_detect("http", h_url)) {
    h_url <- paste0("http://zillow.com", h_url, "/?fullpage=TRUE")
  }
  
  rD <- rsDriver(browser = "chrome")
  pgdrive <- rD[["client"]]
  try(pgdrive$navigate(h_url))
  
  src <- pgdrive$getPageSource() %>% magrittr::extract2(1) %>% read_html()
  pgdrive$close()
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
    summarize(value = value %>% str_split(", ?") %>% unlist %>% unique() %>% paste(collapse = ", ")) %>%
    tidyr::spread(key = name, value = value)
  
  photos <- html_nodes(src, ".img-wrapper img") %>% html_attrs() %>%
    map_df(.f = function(z) {
      names(z) <- str_replace(names(z), "href|src", "link")
      data_frame(link = z["link"], id = z["id"], class = z["class"], filename = str_extract(link, "[A-z0-9]*.[a-z]{3,4}$"))
    })
  
  list(facts = bind_cols(facts_named_df, facts_glance), photos = photos)
  
}

rD <- rsDriver(browser = "chrome")
remDr <- rD[["client"]]
url <- "https://www.zillow.com/homes/for_sale/fsba,fsbo_lt/house_type/ef51a5aedfX1-CR1n2kaelgv03ge_12pfr1_crid/3-_beds/21780-_lot/0_singlestory/42.370212,-93.457261,41.735966,-94.3499_rect/9_zm/1_p/0_mmm/"
remDr$navigate(url)
# webElem <- remDr$findElements(using = 'tag name', value = "article")

pgsrc <- remDr$getPageSource() %>% magrittr::extract2(1) %>% read_html()
# Close driver initially
remDr$close()

articles <- html_nodes(pgsrc, "article") 
house_info <- articles %>% 
  html_attrs() %>%
  lapply(., function(x) as.data.frame(t(x))) %>%
  bind_rows() %>%
  set_names(make.names(names(.)) %>%
              str_replace("data\\.", "")) %>%
  select(photocount, id = zpid, latitude, longitude, sale_type = sgapt) %>%
  mutate_at(.vars = vars(c("latitude", "longitude")), .funs = function(x) as.numeric(x)/1000000)

iteminfo <- html_nodes(pgsrc, "article .zsg-photo-card-content") %>%
  map_df(parse_iteminfo)

houses <- full_join(iteminfo, house_info)

houses_fullinfo <- map(houses$link, get_houseinfo) 

library(magrittr)
houses_fullinfo %<>% bind_rows()

houses_pics <- select(houses_fullinfo, id, photos) %>%
  unnest()

houses_fullinfo <- bind_rows(houses, houses_fullinfo)
if (!exists("house_info.csv")) {
  write_csv(houses_fullinfo, "house_info.csv")
} else {
  other_info <- read_csv("house_info.csv")
  bind_rows(other_info, houses_fullinfo) %>%
    write_csv("house_info.csv")
}
