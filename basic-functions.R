
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
    h_url <- paste0("http://zillow.com", h_url, "?fullpage=TRUE")
  }
  # remDr$navigate(h_url)
  # btn <- remDr$findElement(using = 'xpath', value = '//*[contains(text(),"Expand")]')
  # remDr$mouseMoveToLocation(webElement = btn)
  # remDr$click()
  # src <- remDr$getPageSource() %>% magrittr::extract2(1) %>% read_html()
  
  src <- read_html(h_url)
  desc <- html_nodes(src, ".hdp-header-description .zsg-content-item") %>% html_text()
  
  facts_glance <- html_nodes(src, ".zsg-media-bd .fact-value") %>% 
    html_text(trim = T) %>% t() %>% as_data_frame() %>%
    set_names(html_nodes(src, ".zsg-media-bd .hdp-fact-ataglance-heading") %>% 
                html_text(trim = T))
  facts_full <- html_nodes(src, ".home-details-facts-container .fact-container") %>% html_text()
  
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
  
  photos <- html_nodes(src, ".photo-tile-image") %>% html_attrs() %>%
    map_df(.f = function(z) {
      names(z) <- str_replace(names(z), "href|src", "link")
      data_frame(id = id, link = z["link"], photo_id = z["id"], class = z["class"], filename = paste0(photo_dir, "/", str_extract(link, "[A-z0-9]*.[a-z]{3,4}$")))
    })
  
  list(facts = bind_cols(facts_named_df, facts_glance), photos = photos)
  
}