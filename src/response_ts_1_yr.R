#' ---
#' title: Prop Capture, response of GIA prop, non-GIA prop
#' author: Han Zhang
#' output:
#'    html_document:
#'      toc: true
#' ---
#+ echo=F
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(stringr)
library("reshape2")
# --------  1. Data Load --------- #
#' # Introduction
#'
#' In this section, I will focus on analyzing GIA % change over time first. Then I will look at other MM fund.
#'
#' load data of 09-2019 to 04-2020
#'

# Pull one year data: 8 files
cft_one_yr_names <- paste("./data/",
  list.files(
    path = "./data/",
    pattern = "^cfr_[0-9]"
  ),
  sep = ""
)
var_int <- c(
  "PlatformName", 
  "ICU", 
  "ContractNo",
  "EffectiveDate", 
  "ProductType", 
  "PlatformID",
  "DCorDB", 
  "Segment", 
  "Asset Segment Size", 
  "TotalContractAssets",
  "GIA", 
  "Plan Count", 
  "PptCount",
  "% of MM Target Date & Risk Assets", 
  "% of MM Stable Value Assets",
  "% of MM Strategic Funds", 
  "% of MM Index Assets",
  "Total Proprietary Assets %"
)
cft_one_yr_files <- lapply(cft_one_yr_names, function(i) {
  date_part <- gsub("_", "/", str_extract(i, "[:digit:]{1,2}\\_[:digit:]{1,2}\\_[:digit:]{1,4}"))
  cat("\n", date_part, "\n")
  data.frame(cbind(date = date_part, read.csv(i,
    na.strings = c("NA", "-", "", ".", NULL, " "), header = TRUE,
    colClasses = c("character"), check.names = F
  )[, c(var_int)]))
})

## look at column names match
col_names_cft_one_yr <- lapply(cft_one_yr_files, function(i) {
  names(i)
})
identicalValue <- function(x, y) if (identical(x, y)) x else FALSE
Reduce(identicalValue, col_names_cft_one_yr) # if print the column names, then it indicates they are all have the same col. names

## stack data together
mydata_one_yr <- do.call("rbind", cft_one_yr_files)
## name format
names(mydata_one_yr) <- tolower(gsub(x = names(mydata_one_yr), pattern = "\\.", replacement = "_"))
## DC only
mydata_one_year <- mydata_one_yr %>% filter(dcordb == "DC", platformid %in% c("1", "2", "3"))

dim(mydata_one_yr)
dim(mydata_one_year)

#' # Prop Fund Performance on Asset Size and Segment level
#'
#' ## Prop Fund overall performance
#'

tab <- mydata_one_year %>%
  group_by(date) %>%
  summarise(
    "total_asset(B)" = round(sum(as.numeric(totalcontractassets)) / 1000000000, 1),
    "gia_p(%)" = round(sum(as.numeric(gia)) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "gia_amount(B)" = round(sum(as.numeric(gia)) / 1000000000, 1),
    "non_gia_prop_p(%)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "non_gia_prop(B)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / 1000000000, 1)
  ) %>%
  arrange(as.Date(date, "%m/%d/%Y"))

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")
#'
#' ### GIA summary by date
#'
#+ fig.width=12

g <- ggplot(tab, aes(x = date, y = `gia_p(%)` * 10)) +
  geom_bar(mapping = aes(x = date, y = `total_asset(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  geom_text(aes(label = `gia_p(%)`, x = date, y = `gia_p(%)` * 10), colour = "blue", vjust = 2) +
  geom_text(aes(label = `total_asset(B)`, x = date, y = `total_asset(B)`), colour = "black", vjust = 2) +
  #  scale_x_date(date_labels="%m-%d-%Y", name = "date", date_breaks = "1 month") +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . / 10,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab, aes(x = date, y = `gia_p(%)`)) +
  geom_bar(mapping = aes(x = date, y = `gia_amount(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  geom_text(aes(label = `gia_p(%)`, x = date, y = `gia_p(%)`), colour = "blue", vjust = 2) +
  geom_text(aes(label = `gia_amount(B)`, x = date, y = `gia_amount(B)`), colour = "black", vjust = 2) +
  #  scale_x_date(date_labels="%m-%d-%Y", name = "date", date_breaks = "1 month") +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Asset (B)",
    sec.axis = sec_axis(~.,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

#'
#' ### non-GIA prop summary by date
#'

#+ fig.width=12

g <- ggplot(tab, aes(x = date, y = `non_gia_prop_p(%)` * 5)) +
  geom_bar(mapping = aes(x = date, y = `total_asset(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  geom_text(aes(label = `non_gia_prop_p(%)`, x = date, y = `non_gia_prop_p(%)` * 5), colour = "blue", vjust = 2) +
  geom_text(aes(label = `total_asset(B)`, x = date, y = `total_asset(B)`), colour = "black", vjust = 2) +
  #  scale_x_date(date_labels="%m-%d-%Y", name = "date", date_breaks = "1 month") +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . / 5,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab, aes(x = date, y = `non_gia_prop_p(%)`)) +
  geom_bar(mapping = aes(x = date, y = `non_gia_prop(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  geom_text(aes(label = `non_gia_prop_p(%)`, x = date, y = `non_gia_prop_p(%)`), colour = "blue", vjust = 2) +
  geom_text(aes(label = `non_gia_prop(B)`, x = date, y = `non_gia_prop(B)`), colour = "black", vjust = 2) +
  #  scale_x_date(date_labels="%m-%d-%Y", name = "date", date_breaks = "1 month") +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Prop Asset (B)",
    sec.axis = sec_axis(~.,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

#' ## Prop Fund by Segment
#'

tab <- mydata_one_year %>%
  mutate(segments = case_when(
    segment %in% c("Corporate", "PEO") ~ "1. Corporate",
    segment == "Government" ~ "2. Government",
    segment == "Not For Profit" ~ "3. NFP",
    segment == "Taft Hartley" ~ "4. Taft Hartley",
    TRUE ~ paste("5.", segment, sep = " ")
  )) %>%
  ungroup() %>%
  group_by(date, segments) %>%
  summarise(
    "total_asset(B)" = round(sum(as.numeric(totalcontractassets)) / 1000000000, 1),
    "gia_p(%)" = round(sum(as.numeric(gia)) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "gia_amount(B)" = round(sum(as.numeric(gia)) / 1000000000, 3),
    "non_gia_prop_p(%)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "non_gia_prop(B)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / 1000000000, 1)
  ) %>%
  arrange(segments, as.Date(date, "%m/%d/%Y"))

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")

#'
#' ### GIA
#'
#+ fig.width=12
g <- ggplot(tab, aes(y = `gia_p(%)` * 5, x = date, color = segments)) +
  geom_bar(mapping = aes(x = tab$date, y = tab$"total_asset(B)", color = segments), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . / 5,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab, aes(y = `gia_p(%)` / 2, x = date, color = segments)) +
  geom_bar(mapping = aes(x = date, y = `gia_amount(B)`, color = segments), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Asset (B)",
    sec.axis = sec_axis(~ . * 2,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )


#'
#' ### non-GIA prop
#'
#+ fig.width=12
g <- ggplot(tab, aes(y = `non_gia_prop_p(%)` * 2, x = date, color = segments)) +
  geom_bar(mapping = aes(x = tab$date, y = tab$"total_asset(B)", color = segments), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . / 2,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab, aes(y = `non_gia_prop_p(%)` / 2, x = date, color = segments)) +
  geom_bar(mapping = aes(x = date, y = `non_gia_prop(B)`, color = segments), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non GIA Prop Asset (B)",
    sec.axis = sec_axis(~ . * 2,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

#' ## Prop Fund by Asset Size
#'

tab <- mydata_one_year %>%
  mutate(asset_size = case_when(
    asset_segment_size %in% c("$0 - $250K", "0-250K") ~ "a. $0 - $250K",
    asset_segment_size %in% c("$250K - $1M", "250K-1M") ~ "b. $250K - $1M",
    asset_segment_size %in% c("$1 - $5M", "1-5M") ~ "c. $1 - $5M",
    asset_segment_size %in% c("$5M -$15M", "5-15M") ~ "d. $5M -$15M",
    asset_segment_size %in% c("$15M - $75M", "15-75M") ~ "e. $15M - $75M",
    asset_segment_size %in% c("$75M - $150M", "75-150M") ~ "f. $75M - $150M",
    asset_segment_size %in% c("$150M+", "150M+") ~ "g. $150M+",
    TRUE ~ "h. Other"
  )) %>%
  ungroup() %>%
  group_by(asset_size, date) %>%
  summarise(
    "total_asset(B)" = round(sum(as.numeric(totalcontractassets)) / 1000000000, 1),
    "gia_p(%)" = round(sum(as.numeric(gia)) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "gia_amount(B)" = round(sum(as.numeric(totalcontractassets)) * sum(as.numeric(gia)) / sum(as.numeric(totalcontractassets)) / 1000000000, 3),
    "non_gia_prop_p(%)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "non_gia_prop(B)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / 1000000000, 1)
  ) %>%
  arrange(asset_size, as.Date(date, "%m/%d/%Y"))

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")


#'
#' ### GIA
#'

#+ fig.width=12
g <- ggplot(tab, aes(y = `gia_p(%)` * 8, x = date, color = asset_size)) +
  geom_bar(mapping = aes(x = date, y = `total_asset(B)`, color = asset_size), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . / 8,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab, aes(y = `gia_p(%)`, x = date, color = asset_size)) +
  geom_bar(mapping = aes(x = date, y = `gia_amount(B)`, color = asset_size), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Asset (B)",
    sec.axis = sec_axis(~.,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )


#'
#' ### non-GIA prop
#'

#+ fig.width=12
g <- ggplot(tab, aes(y = `non_gia_prop_p(%)` * 4, x = date, color = asset_size)) +
  geom_bar(mapping = aes(x = date, y = `total_asset(B)`, color = asset_size), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . / 4,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab, aes(y = `non_gia_prop_p(%)`, x = date, color = asset_size)) +
  geom_bar(mapping = aes(x = date, y = `non_gia_prop(B)`, color = asset_size), stat = "identity", fill = "grey95") +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non GIA Prop Asset (B)",
    sec.axis = sec_axis(~.,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )




#' ## Prop Fund by Segment and asset size
#'

tab <- mydata_one_year %>%
  mutate(
    asset_size = case_when(
      asset_segment_size %in% c("$0 - $250K", "0-250K") ~ "a. $0 - $250K",
      asset_segment_size %in% c("$250K - $1M", "250K-1M") ~ "b. $250K - $1M",
      asset_segment_size %in% c("$1 - $5M", "1-5M") ~ "c. $1 - $5M",
      asset_segment_size %in% c("$5M -$15M", "5-15M") ~ "d. $5M -$15M",
      asset_segment_size %in% c("$15M - $75M", "15-75M") ~ "e. $15M - $75M",
      asset_segment_size %in% c("$75M - $150M", "75-150M") ~ "f. $75M - $150M",
      asset_segment_size %in% c("$150M+", "150M+") ~ "g. $150M+",
      TRUE ~ "h. Other"
    ),
    segments = case_when(
      segment %in% c("Corporate", "PEO") ~ "1. Corporate",
      segment == "Government" ~ "2. Government",
      segment == "Not For Profit" ~ "3. NFP",
      segment == "Taft Hartley" ~ "4. Taft Hartley",
      TRUE ~ paste("5.", segment, sep = " ")
    )
  ) %>%
  ungroup() %>%
  group_by(date, segments, asset_size) %>%
  summarise(
    "total_asset(B)" = round(sum(as.numeric(totalcontractassets)) / 1000000000, 1),
    "gia_p(%)" = round(sum(as.numeric(gia)) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "gia_amount(B)" = round(sum(as.numeric(totalcontractassets)) * sum(as.numeric(gia)) / sum(as.numeric(totalcontractassets)) / 1000000000, 3),
    "non_gia_prop_p(%)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / sum(as.numeric(totalcontractassets)) * 100, 1),
    "non_gia_prop(B)" = round((sum(as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__)) - sum(as.numeric(gia))) / 1000000000, 1)
  ) %>%
  arrange(segments, asset_size, as.Date(date, "%m/%d/%Y"))

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")
tab$group <- paste(tab$segments, tab$asset_size, sep = "-")

#'
#' ### GIA
#'
#+ fig.width=12
g <- ggplot(tab, aes(y = `gia_p(%)`, x = date)) +
  geom_bar(mapping = aes(x = date, y = `total_asset(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  facet_grid(segments ~ asset_size) +
  # geom_text(aes(label=`gia_p(%)`, x=date, y=`gia_p(%)`), colour="blue", vjust = -2)+
  # geom_text(aes(label=`total_asset(B)`, x=date, y=`total_asset(B)`), colour="black", vjust = 0)+
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~.,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 24)
  )

g

g <- ggplot(tab, aes(y = `gia_p(%)` / 10, x = date)) +
  geom_bar(mapping = aes(x = date, y = `gia_amount(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  facet_grid(segments ~ asset_size) +
  # geom_text(aes(label=`gia_p(%)`, x=date, y=`gia_p(%)`), colour="blue", vjust = -2)+
  # geom_text(aes(label=`total_asset(B)`, x=date, y=`total_asset(B)`), colour="black", vjust = 0)+
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Asset (B)",
    sec.axis = sec_axis(~ . * 10,
      name = "GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 24)
  )

g



#'
#' ### non-GIA prop
#'
#+ fig.width=12
g <- ggplot(tab, aes(y = `non_gia_prop_p(%)` / 2, x = date)) +
  geom_bar(mapping = aes(x = date, y = `total_asset(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  facet_grid(segments ~ asset_size) +
  # geom_text(aes(label=`gia_p(%)`, x=date, y=`gia_p(%)`), colour="blue", vjust = -2)+
  # geom_text(aes(label=`total_asset(B)`, x=date, y=`total_asset(B)`), colour="black", vjust = 0)+
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "Total Asset (B)",
    sec.axis = sec_axis(~ . * 2,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 24)
  )

g

g <- ggplot(tab, aes(y = `non_gia_prop_p(%)` / 10, x = date)) +
  geom_bar(mapping = aes(x = date, y = `non_gia_prop(B)`), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(size = 0.4, color = "blue") +
  facet_grid(segments ~ asset_size) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non GIA Prop Asset (B)",
    sec.axis = sec_axis(~ . * 10,
      name = "non-GIA prop (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 24)
  )

g


#'
#' # Prop Fund Performance on Contract Level
#'
#' * Data preperation
#'

mydata_one_year <- mydata_one_yr %>% filter(dcordb == "DC", platformid %in% c("1", "2", "3"))

dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  mutate(
    asset_size = case_when(
      asset_segment_size %in% c("$0 - $250K", "0-250K") ~ "a. $0 - $250K",
      asset_segment_size %in% c("$250K - $1M", "250K-1M") ~ "b. $250K - $1M",
      asset_segment_size %in% c("$1 - $5M", "1-5M") ~ "c. $1 - $5M",
      asset_segment_size %in% c("$5M -$15M", "5-15M") ~ "d. $5M -$15M",
      asset_segment_size %in% c("$15M - $75M", "15-75M") ~ "e. $15M - $75M",
      asset_segment_size %in% c("$75M - $150M", "75-150M") ~ "f. $75M - $150M",
      asset_segment_size %in% c("$150M+", "150M+") ~ "g. $150M+",
      TRUE ~ "h. Other"
    ),
    segments = case_when(
      segment %in% c("Corporate", "PEO") ~ "1. Corporate",
      segment == "Government" ~ "2. Government",
      segment == "Not For Profit" ~ "3. NFP",
      segment == "Taft Hartley" ~ "4. Taft Hartley",
      TRUE ~ paste("5.", segment, sep = " ")
    ),
    gia_p = round(as.numeric(gia) / as.numeric(totalcontractassets) * 100, 1),
    non_gia = as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__) - as.numeric(gia),
    non_gia_p = round((as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__) - as.numeric(gia)) / as.numeric(totalcontractassets) * 100, 1)
  ) %>%
  distinct_at(vars(date, contractno, icu, platformname, segments, asset_size, totalcontractassets, total_proprietary_assets__, gia, gia_p, non_gia, non_gia_p))
dim(mydata_one_year)

#' Contracts have distinct contract No, icu, segments, asset size, total contract amount, gia, gia
#' percentage, non-gia, and non-gia percentage at each time

tab <- mydata_one_year %>%
  ungroup() %>%
  group_by(date, contractno, icu) %>%
  summarise(total = n())

table(tab$total)

#+ warning = F
tab %>%
  filter(total == 2) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#' There is one contract appears twice on 3-31-2020, it has two rows. Ignore it later.


#'
#' * Percentage of policies that exist in all 8 months
#'

tab <- mydata_one_year %>%
  ungroup() %>%
  group_by(contractno, icu) %>%
  summarise(total = n())

#+ warning = F
table(tab$total)
round(prop.table(table(tab$total)) * 100, 1)

#' There are ``r as.numeric(round(prop.table(table(tab$total))*100,1)[8])``%
#' of policies that appears in all eight months, I will focus on those
#' in the following study.
#'
#' Add the number of appearance back to the data
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  left_join(tab, by = c("contractno", "icu"))
dim(mydata_one_year)


#' * For those policies that exist in all 8 months, how their asset size
#' and segments change

dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  filter(total == 8)
dim(mydata_one_year)


tab_p <- mydata_one_year %>%
  ungroup() %>%
  group_by(contractno, icu) %>%
  summarize(
    segments_gp = length(unique(segments)),
    asset_gp = length(unique(asset_size))
  )

#+ warning = F
table(tab_p$segments_gp)
table(tab_p$asset_gp)
#' Segments features are stable, around 99.9\% of the policies that are belong to the same sector,
#' Moving forward, I will ignore those policies that change segments.
#' The asset size of policies could change, which is not too suprising.
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  left_join(tab_p, by = c("contractno", "icu"))
dim(mydata_one_year)

#' Only consider policies that do not change segments.
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  filter(segments_gp == 1)
dim(mydata_one_year)


#' Make sure non-gia is valid
#'
summary(mydata_one_year$non_gia_p)
summary(mydata_one_year$non_gia)
#' There are 0.2% of policies that have invalid non-gia amount, I will correct them roughly for now, for
#' it is not that important here. To be specific, I will treat all negative non-gia as 0 non-gia.
#'
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  mutate(
    non_gia = case_when(
      non_gia <= 0 ~ 0,
      TRUE ~ non_gia
    ),
    non_gia_p = case_when(
      non_gia <= 0 ~ 0,
      TRUE ~ non_gia_p
    )
  )
dim(mydata_one_year)

summary(mydata_one_year$non_gia_p)
summary(mydata_one_year$non_gia)

summary(mydata_one_year$gia_p)
summary(as.numeric(mydata_one_year$gia))

a <- mydata_one_year
# mydata_one_year <- a
#'
#' ## GIA on Contract Level
#'
#' * For each contract, compute its GIA% group (20 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group gia_p means vary
#' over months (8 months)
#'


dim(mydata_one_year)
mydata_b <- mydata_one_year %>%
  filter(date == "12/31/2019")
seq_f <- quantile(mydata_b$gia_p[which(mydata_b$gia_p != 0 & mydata_b$gia_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_one_year <- mydata_one_year %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(gia_p_gp = cut(gia_p,
    breaks = c(
      -0.1,
      0,
      seq_f,
      100
    ),
    right = T
  )) %>%
  select(contractno, icu, gia_p_gp) %>%
  right_join(mydata_one_year, by = c("contractno", "icu"))
dim(mydata_one_year)


#' Track GIA Group mean change in each of 8 months
tab <- mydata_one_year %>%
  group_by(date, gia_p_gp) %>%
  summarize(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_p_md = median(gia_p),
    gia_p_min = min(gia_p),
    gia_p_max = max(gia_p)
  )

#+ warning = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, gia_p_gp, total, gia_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)



tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")


#' smaller group
tab_s <- mydata_one_year %>%
  filter(gia_p_gp %in% c(
    "(-0.1,0]",
    "(0,0.1]",
    "(0.1,0.5]",
    "(0.5,1.6]",
    "(1.6,3.2]",
    "(3.2,5.4]"
  )) %>%
  group_by(date, gia_p_gp) %>%
  summarize(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_p_md = median(gia_p),
    gia_p_min = min(gia_p),
    gia_p_max = max(gia_p)
  )
tab_s$date <- as.Date(as.character(tab_s$date), format = "%m/%d/%Y")

#' ### GIA Group Means
#'
#' based on mean
#+ fig.width=12
g <- ggplot(tab, aes(y = gia_p_avg, x = date, color = gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = gia_p_gp)) +
  geom_bar(mapping = aes(x = gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("GIA Group") +
  labs(caption = "GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))


#' based on mean: smaller group
#+ fig.width=12
g <- ggplot(tab_s, aes(y = gia_p_avg, x = date, color = gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab_s %>% filter(date == "2019-12-31"), aes(y = total, x = gia_p_gp)) +
  geom_bar(mapping = aes(x = gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("GIA Group") +
  labs(caption = "GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))


#' ### GIA Group Medians
#'
#' based on median
#+ fig.width=12
g <- ggplot(tab, aes(y = gia_p_md, x = date, color = gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = gia_p_gp)) +
  geom_bar(mapping = aes(x = gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("GIA Group") +
  labs(caption = "GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))



#' based on median: smaller group
#+ fig.width=12
g <- ggplot(tab_s, aes(y = gia_p_md, x = date, color = gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab_s %>% filter(date == "2019-12-31"), aes(y = total, x = gia_p_gp)) +
  geom_bar(mapping = aes(x = gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("GIA Group") +
  labs(caption = "GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' ### GIA Groups Maxes
#'
#' based on max
#+ fig.width=12
g <- ggplot(tab, aes(y = gia_p_max, x = date, color = gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Max %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = gia_p_gp)) +
  geom_bar(mapping = aes(x = gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("GIA Group") +
  labs(caption = "GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' ### GIA Group Mins
#'
#' based on min
#+ fig.width=12
g <- ggplot(tab, aes(y = gia_p_min, x = date, color = gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "GIA Min %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = gia_p_gp)) +
  geom_bar(mapping = aes(x = gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("GIA Group") +
  labs(caption = "GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#'
#' ### GIA Groups and GIA Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the GIA
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#'

tab <- mydata_one_year %>%
  ungroup() %>%
  group_by(contractno, icu) %>%
  summarise(
    total = n(),
    gia_p_min = min(gia_p),
    gia_p_md = median(gia_p),
    gia_p_avg = mean(gia_p),
    gia_p_max = max(gia_p),
    gia_p_gap = round(gia_p_max - gia_p_min, 2)
  )

seq_gap <- quantile(tab$gia_p_gap[which(tab$gia_p_gap != 0 & tab$gia_p_gap != 100)], probs = seq(0, 1, 0.1))

tab_t <- tab %>%
  ungroup() %>%
  mutate(gia_gap_gp = cut(gia_p_gap,
    breaks = c(
      -0.1,
      0,
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, icu, gia_gap_gp, gia_p_gap)

#' see what distribution looks like in gia gap

#+ warning = F
table(tab_t$gia_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
prop.table(table(tab_t$gia_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#' add gia gap distribution of each contract back to original data
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  left_join(tab_t, by = c("contractno", "icu"))
dim(mydata_one_year)

tab <- mydata_one_year %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#'
#'

ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Number of Contract") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Consistency") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Number of Contract Distribution") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Consistency") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

#'
#'
#'
#' * QC the summarization in above step
#'
#' select a representative in each of the group based on GIA_p_gp and GIA_gap_gp
#' on the 12-31-2019
#'

qc_gia <- mydata_one_year %>%
  ungroup() %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  slice(n()) %>%
  select(contractno, icu, gia_p_gp, gia_gap_gp)

qc_gia_data <- mydata_one_year %>%
  filter(
    contractno %in% qc_gia$contractno,
    icu %in% qc_gia$icu
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, icu, gia_p, gia_p_gp, gia_p_gap, gia_gap_gp) %>%
  arrange(contractno, icu, as.Date(date))

#' After looking some cases, the features of GIA percent groups are correct.
#'
#' ## non-GIA on Contract Level
#'
#'
#' * For each contract, compute its GIA% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group gia_p means vary
#' over months (8 months)
#'

b <- mydata_one_year
# mydata_one_year <- b
dim(mydata_one_year)
mydata_b <- mydata_one_year %>%
  filter(date == "12/31/2019")
seq_f <- quantile(mydata_b$non_gia_p[which(mydata_b$non_gia_p != 0 & mydata_b$non_gia_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_one_year <- mydata_one_year %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(non_gia_p_gp = cut(non_gia_p,
    breaks = c(
      -0.1,
      0,
      seq_f,
      100
    ),
    right = T
  )) %>%
  select(contractno, icu, non_gia_p_gp) %>%
  right_join(mydata_one_year, by = c("contractno", "icu"))
dim(mydata_one_year)

summary(mydata_one_year$non_gia_p_gp)
#' Track GIA Group mean change in each of 8 months
tab <- mydata_one_year %>%
  group_by(date, non_gia_p_gp) %>%
  summarize(
    total = n(),
    non_gia_p_avg = round(mean(non_gia_p), 2),
    non_gia_p_md = median(non_gia_p),
    non_gia_p_min = min(non_gia_p),
    non_gia_p_max = max(non_gia_p)
  )

#+ warning = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, non_gia_p_gp, total, non_gia_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")


#' smaller group
tab_s <- mydata_one_year %>%
  filter(non_gia_p_gp %in% c(
    "(-0.1,0]",
    "(0,0.1]",
    "(0.1,1.6]",
    "(1.6,4.4]"
  )) %>%
  group_by(date, non_gia_p_gp) %>%
  summarize(
    total = n(),
    non_gia_p_avg = round(mean(non_gia_p), 2),
    non_gia_p_md = median(non_gia_p),
    non_gia_p_min = min(non_gia_p),
    non_gia_p_max = max(non_gia_p)
  )
tab_s$date <- as.Date(as.character(tab_s$date), format = "%m/%d/%Y")

#' ### non-GIA Group Means
#'
#' based on mean
#+ fig.width=12
g <- ggplot(tab, aes(y = non_gia_p_avg, x = date, color = non_gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "non-GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = non_gia_p_gp)) +
  geom_bar(mapping = aes(x = non_gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("non-GIA Group") +
  labs(caption = "non-GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' based on mean: smaller group
#+ fig.width=12
g <- ggplot(tab_s, aes(y = non_gia_p_avg, x = date, color = non_gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "non-GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab_s %>% filter(date == "2019-12-31"), aes(y = total, x = non_gia_p_gp)) +
  geom_bar(mapping = aes(x = non_gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("non-GIA Group") +
  labs(caption = "non-GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' ### non-GIA Group Medians
#'
#' based on median
#+ fig.width=12
g <- ggplot(tab, aes(y = non_gia_p_md, x = date, color = non_gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "non-GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = non_gia_p_gp)) +
  geom_bar(mapping = aes(x = non_gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("non-GIA Group") +
  labs(caption = "non-GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' based on median: smaller group
#+ fig.width=12
g <- ggplot(tab_s, aes(y = non_gia_p_md, x = date, color = non_gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "non-GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab_s %>% filter(date == "2019-12-31"), aes(y = total, x = non_gia_p_gp)) +
  geom_bar(mapping = aes(x = non_gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("non-GIA Group") +
  labs(caption = "non-GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' ### non-GIA Groups Maxes
#'
#' based on max
#+ fig.width=12
g <- ggplot(tab, aes(y = non_gia_p_max, x = date, color = non_gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Max %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "non-GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = non_gia_p_gp)) +
  geom_bar(mapping = aes(x = non_gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("non-GIA Group") +
  labs(caption = "non-GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' ### non-GIA Group Mins
#'
#' based on min
#+ fig.width=12
g <- ggplot(tab, aes(y = non_gia_p_min, x = date, color = non_gia_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "non-GIA Min %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "non-GIA Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = non_gia_p_gp)) +
  geom_bar(mapping = aes(x = non_gia_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("non-GIA Group") +
  labs(caption = "non-GIA Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#'
#' ### non-GIA Groups and non-GIA Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the non-GIA
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#'

tab <- mydata_one_year %>%
  ungroup() %>%
  group_by(contractno, icu) %>%
  summarise(
    total = n(),
    non_gia_p_min = min(non_gia_p),
    non_gia_p_md = median(non_gia_p),
    non_gia_p_avg = mean(non_gia_p),
    non_gia_p_max = max(non_gia_p),
    non_gia_p_gap = round(non_gia_p_max - non_gia_p_min, 2)
  )

seq_gap <- quantile(tab$non_gia_p_gap[which(tab$non_gia_p_gap != 0 & tab$non_gia_p_gap != 100)], probs = seq(0, 1, 0.1))

tab_t <- tab %>%
  ungroup() %>%
  mutate(non_gia_gap_gp = cut(non_gia_p_gap,
    breaks = c(
      -0.1,
      0,
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, icu, non_gia_gap_gp, non_gia_p_gap)

#' see what distribution looks like in gia gap

#+ warning = F
table(tab_t$non_gia_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
prop.table(table(tab_t$non_gia_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#' add gia gap distribution of each contract back to original data
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  left_join(tab_t, by = c("contractno", "icu"))
dim(mydata_one_year)

tab <- mydata_one_year %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(non_gia_p_gp, non_gia_gap_gp) %>%
  summarise(
    total = n(),
    non_gia_p_avg = round(mean(non_gia_p), 2),
    non_gia_gap_avg = round(mean(as.numeric(non_gia_p_gap)), 2),
    non_gia_gap_median = round(median(as.numeric(non_gia_p_gap)), 2),
  )

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#'
#'

ggplot(tab, aes(y = total, x = non_gia_p_gp, fill = non_gia_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline non-GIA Group of 12/31/2019") +
  ylab("Number of Contract") +
  labs(fill = "non-GIA Gaps in 8 Months") +
  ggtitle("non-GIA Consistency") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = non_gia_p_gp, fill = non_gia_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline non-GIA Group of 12/31/2019") +
  ylab("Number of Contract Distribution") +
  labs(fill = "non-GIA Gaps in 8 Months") +
  ggtitle("non-GIA Consistency") +
  theme(
    axis.text.x = element_text(angle = 90)
  )
#'
#'
#'
#' * QC the summarization in above step
#'
#' select a representative in each of the group based on non_gia_p_gp and non_gia_gap_gp
#' on the 12-31-2019
#'

qc_non_gia <- mydata_one_year %>%
  ungroup() %>%
  group_by(non_gia_p_gp, non_gia_gap_gp) %>%
  slice(n()) %>%
  select(contractno, icu, non_gia_p_gp, non_gia_gap_gp)

qc_non_gia_data <- mydata_one_year %>%
  filter(
    contractno %in% qc_non_gia$contractno,
    icu %in% qc_non_gia$icu
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, icu, non_gia_p, non_gia_p_gp, non_gia_p_gap, non_gia_gap_gp) %>%
  arrange(contractno, icu, as.Date(date))

#' After looking some cases, the features of GIA percent groups are correct.


#'
#' # Prop Fund Inflated 0% Group
#'
#'
dim(mydata_one_year)
mydata_one_year_12_31_19 <- mydata_one_year %>%
  filter(date == "12/31/2019")
dim(mydata_one_year_12_31_19)


a <- mydata_one_year_12_31_19 %>%
  mutate(
    gia_sg = case_when(
      gia_p < 0.01 ~ 0,
      gia_p > 99 ~ 100,
      TRUE ~ 50
    ),
    non_gia_sg = case_when(
      non_gia_p < 0.01 ~ 0,
      non_gia_p > 99 ~ 100,
      TRUE ~ 50
    )
  )
table(a$gia_sg, a$non_gia_sg)

table(a$gia_sg, a$non_gia_sg, a$platformname)
#' Both GIA and non-GIA are enriched by 0.

#'
#' ## Inflated 0% GIA
#'
#' split data into three parts as mentioned above.
#'
mydata_one_year_12_31_19 %>%
  mutate(gia_sg = case_when(
    gia_p < 0.01 ~ 0,
    gia_p > 99 ~ 100,
    TRUE ~ 50
  )) %>%
  ungroup() %>%
  group_by(gia_sg) %>%
  summarise(total = n())
#' There are around 41.0% of policies that have 0 GIA. And there are
#' only 2.79% of policies that have 100\% GIA. Thus, the 0 GIA policies
#' are outliers and form a large group, whereas 100\% GIA is normal.

mydata_one_year_12_31_19 %>%
  mutate(gia_sg = case_when(
    gia_p < 0.01 ~ 0,
    gia_p > 99 ~ 100,
    TRUE ~ 50
  )) %>%
  ungroup() %>%
  group_by(gia_sg, platformname) %>%
  summarise(total = n())

#' There are 75.8% of policies with 0\% GIA come
#' from TRAC platform. And 47.7% of TRAC policies have 0 GIAs, and OMNI platform
#' generates 20.8%, and ReFlex generates 35.8%. It implies that 0 GIA form a large
#' group no matter which platform the policy comes from, while TRAC is the main driver
#' of it.
#'
#' There are 72.7% of policies with 100\% GIA come
#' from OMNI platform. And around 12.6\% of OMNI policies have 100% GIAs, while other platforms
#' have less than 1\% of their policies generate 100% GIAs. It implies that 100% GIA group does
#' not form a very big group and we could treat them as a normal group, although OMNI platform
#' generates a relatively large amount of such policies.
#'
#'
#' From above, it seems that platform plays a role in terms of those 0 GIA group of policies.
#' Is there any underlying differences/common among those platforms?

#'
#' ## Inflated 0% non-GIA prop
#'
#'
#' split data into three parts as mentioned above.
#'
mydata_one_year_12_31_19 %>%
  mutate(non_gia_sg = case_when(
    non_gia_p < 0.01 ~ 0,
    non_gia_p > 99 ~ 100,
    TRUE ~ 50
  )) %>%
  ungroup() %>%
  group_by(non_gia_sg) %>%
  summarise(total = n())
#' There are around 47.4% of policies that have 0 non-GIA prop rates. And there are
#' only 2.13% of policies that have 100\% GIA. Like GIA, the 0 non-GIA prop policies
#' are outliers and form a large group, whereas 100\% non-GIA is normal.

mydata_one_year_12_31_19 %>%
  mutate(non_gia_sg = case_when(
    non_gia_p < 0.01 ~ 0,
    non_gia_p > 99 ~ 100,
    TRUE ~ 50
  )) %>%
  ungroup() %>%
  group_by(non_gia_sg, platformname) %>%
  summarise(total = n())

#' There are 65.2% of policies with 0\% non-GIA prop come
#' from TRAC platform like GIA. And 47.4% of TRAC policies have 0 GIAs. However, unlike GIA,
#' OMNI platform generates 90.9% , and ReFlex generates 6.3%. It implies that 0 non-GIA prop
#' form a large group in both TRAC and OMNI platforms with OMNI as the main driver, while ReFlex
#' has trivial impact on it. The difference between GIA and non-GIA is apparently negative correlated,
#' and their main difference shows from the platform as well.
#'
#' There are 87.1% of policies with 100\% non-GIA come
#' from TRAC platform unlike GIA where OMNI is the main driver.
#' However, here, there is 0\% of OMNI policies have 100% non-GIA prop, while other platforms
#' have less than 3\% of their policies generate 100% non-GIAs prop. It implies that 100% non-GIA group does
#' not form a very big group and we could treat them as a normal group.
#'
#'




#'
#'
#' # Conclusions
#'
#' As a conclusion, although there is a small proportion of policies
#' whose GIA \% may vary within a year, they do not change as a group
#' level, either from specific features perspecitive or from more
#' granular contract GIA\% group perspecitve.
#'
#' Also, it seems that platform plays a role in terms of those 0's GIA and non-GIA group of policies.
#' Is there any underlying differences/common among those platforms?
