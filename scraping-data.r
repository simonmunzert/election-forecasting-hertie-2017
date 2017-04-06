### -----------------------------
### simon munzert
### intro to web scraping with R
### -----------------------------

## preparations -----------------------

# install packages from CRAN
p_needed <- c("plyr", "dplyr", "stringr", "xml2", "rvest")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)


## Berlin's sister cities ------------------------


# scrape the page source
cities_string <- read_html("https://en.wikipedia.org/wiki/Berlin") %>%
  # extract the list items
  html_nodes(css = ".column-count-3 li") %>%
  html_text()

# remove footnotes ([101], [102], ...) 
cities_string <- str_replace(cities_string, "\\[\\d+\\]", "")

# extract data
year <- str_extract(cities_string, "\\d{4}")
city <- str_extract(cities_string, "[[:alpha:] ]+") %>% str_trim
country <- str_extract(cities_string, "[[:alpha:] ]+$") %>% str_trim

# put everything into data.frame
cities_df <- data.frame(year, city, country)
head(cities_df)

# geocode cities
cities_coords <- geocode(paste0(cities_df$city, ", ", cities_df$country))
cities_df$lon <- cities_coords$lon
cities_df$lat <- cities_coords$lat

# plot cities
pdf(file="sister-cities.pdf", height=4, width=7, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(0, 0, 0, 0))
map_world <- borders("world", colour = "gray50", fill = "white") 
ggplot() + map_world + geom_point(aes(x = cities_df$lon, y = cities_df$lat), color = "red", size = 1) + theme_void()
dev.off()


## Scraping HTML tables ------------------------

# load package
library(rvest)

# import webpage
url_p <- read_html("https://en.wikipedia.org/wiki/List_of_human_spaceflights")
tables <- html_table(url_p, header = TRUE, fill = TRUE)
spaceflights <- tables[[1]]
spaceflights


## Scraping with SelectorGadget ----------------

# scrape headlines from NY Times
url_p <- read_html("https://www.nytimes.com")
headlines <- html_nodes(url_p, css = ".story-heading")
headlines_raw <- html_text(headlines)
head(headlines_raw)
length(headlines_raw)

headlines_clean <- headlines_raw %>% str_replace_all("\\n", "") %>% str_trim()
length(headlines_clean)
str_detect(headlines_clean, "Trump") %>% table()

# scrape data from one of several lists on a page
url_p <- read_html("https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/Homophones")
words_nodes <- html_nodes(url_p, css = ".column-count-4 li")
head(words_nodes)
words_raw <- html_text(words_nodes)
words_split <- str_split(words_raw, pattern = ", and | and |, ")

words_df <- plyr::ldply(words_split, rbind)
head(words_df)


