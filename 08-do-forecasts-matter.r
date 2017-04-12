### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

ghit::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"))
library(tabulizer)
library(magrittr)
library(stringr)
library(dplyr)
library(xtable)

# extract table from first page of example PDF
tab <- extract_tables("WAPOR_FTP_2012_table.pdf", method = "data.frame")

wapor_df <- data.frame(country = lapply(tab, "[", 1) %>% unlist %>% str_trim, 
                       restrictions =  lapply(tab, "[", 2) %>% unlist %>% str_trim,
                       stringsAsFactors = FALSE)

wapor_df <- filter(wapor_df, country != "Country", country != "", restrictions != "")
wapor_df$restricted <- ifelse(str_detect(wapor_df$restrictions, "Yes"), "Yes",
                              ifelse(str_detect(wapor_df$restrictions, "No "), "No", ""))
wapor_df$restricted_days <- str_extract(wapor_df$restrictions, "\\d+") %>% str_trim %>% as.numeric

out_df <- wapor_df %>% select(country, restricted_days) %>% filter(!is.na(restricted_days)) %>% arrange(desc(restricted_days))

# export table
out_df_xtable <- xtable(out_df[1:18,], align = c("r","r", "r"),  caption = "bla", digits = 0) 
names(out_df_xtable) <- c("Country", "Embargo (days)")
print(out_df_xtable, booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!",  include.rownames = FALSE, floating.environment = "table*", file = paste0("wapor_polls_restrictions1.tex"))

out_df_xtable <- xtable(out_df[19:36,], align = c("r","r", "r"),  caption = "bla", digits = 0) 
names(out_df_xtable) <- c("Country", "Embargo (days)")
print(out_df_xtable, booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!",  include.rownames = FALSE, floating.environment = "table*", file = paste0("wapor_polls_restrictions2.tex"))
