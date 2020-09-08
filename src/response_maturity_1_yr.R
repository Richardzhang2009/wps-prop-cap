#' ---
#' title: Prop Capture Maturity Study, response of GIA prop, non-GIA prop
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
#' In this section, I will focus on analyzing GIA % change as product become matured.
#' Then I will look at other MM fund. Data are from month 09-2019 to 04-2020, 8 months.
#' And only policies that has more than $1,000 total asset will be considered.
#'
#'
#+ echo = F, warning = F, results = 'hide'

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
mydata_one_year <- mydata_one_yr %>% filter(
  dcordb == "DC",
  platformid %in% c("1", "2", "3"),
  as.numeric(totalcontractassets) > 1000
)

dim(mydata_one_yr)
dim(mydata_one_year)


# create the days difference and different fund proportion variables
#
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  mutate(
    days_diff = as.Date(as.character(date), format = "%m/%d/%Y") -
      as.Date(as.character(effectivedate), format = "%m/%d/%Y"),
    days_diff_f = case_when(
      as.numeric(days_diff) < 0 ~ 0,
      TRUE ~ as.numeric(days_diff)
    )
  ) %>%
  filter(!is.na(days_diff_f))
dim(mydata_one_year)



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
    discount = case_when(
      as.numeric(total_proprietary_assets__) >= 0.17 ~ "TD Discount",
      TRUE ~ "No Discount"
    ),
    total_asset_value = round(as.numeric(totalcontractassets) / 1000000000, 3),
    gia_p = round(as.numeric(gia) / as.numeric(totalcontractassets) * 100, 1),
    non_gia = as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__) - as.numeric(gia),
    non_gia_p = round((as.numeric(totalcontractassets) * as.numeric(total_proprietary_assets__) - as.numeric(gia)) / as.numeric(totalcontractassets) * 100, 1),
    tdr_p = round(as.numeric(x__of_mm_target_date___risk_assets) * 100, 1),
    sv_p = round(as.numeric(x__of_mm_stable_value_assets) * 100, 1),
    stra_p = round(as.numeric(x__of_mm_strategic_funds) * 100, 1),
    index_p = round(as.numeric(x__of_mm_index_assets) * 100, 1),
    prop_p = round(as.numeric(total_proprietary_assets__) * 100, 1)
  ) %>%
  distinct_at(vars(
    date, effectivedate, contractno, icu, segments, asset_size,
    totalcontractassets, totalcontractassets, total_proprietary_assets__, gia, gia_p,
    non_gia, non_gia_p,
    tdr_p, sv_p, stra_p, index_p,
    prop_p,
    days_diff_f,
    discount,
    total_asset_value
  ))
dim(mydata_one_year)

# ignore policies that prop_p < gia_p
#
dim(mydata_one_year)
mydata_one_year <- mydata_one_year %>%
  filter(as.numeric(prop_p) >= as.numeric(gia_p))
dim(mydata_one_year)

# create days difference group

dim(mydata_one_year)
mydata_one_year_short <- mydata_one_year %>%
  filter(days_diff_f <= 19113)
dim(mydata_one_year_short)
mydata_one_year_short <- mydata_one_year_short %>%
  ungroup() %>%
  mutate(days_diff_gp = cut(days_diff_f,
    breaks = c(
      -0.1,
      30, 60, 90, 120,
      150, 180, 210,
      240, 270, 300, 330,
      seq(365, 19113, 365),
      19113
    ),
    right = T
  )) %>%
  select(date, contractno, icu, totalcontractassets, total_asset_value, contractno, icu, prop_p, gia_p, non_gia_p, tdr_p, sv_p, stra_p, index_p, days_diff_f, days_diff_gp, discount)
dim(mydata_one_year_short)



tab <- mydata_one_year_short %>%
  group_by(days_diff_gp) %>%
  summarise(
    total = n(),
    "total_asset(B)" = sum(total_asset_value),
    prop_avg = round(mean(prop_p), 1),
    gia_avg = round(mean(gia_p), 1),
    non_gia_avg = round(mean(non_gia_p), 1),
    tdr_avg = round(mean(tdr_p), 1),
    sv_avg = round(mean(sv_p), 1),
    stra_avg = round(mean(stra_p), 1),
    index_avg = round(mean(index_p), 1)
  )


tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


# put 20+ years into one group

dim(mydata_one_year)
mydata_one_year_short <- mydata_one_year %>%
  filter(days_diff_f <= 19113)
dim(mydata_one_year_short)
mydata_one_year_short_20 <- mydata_one_year_short %>%
  ungroup() %>%
  mutate(days_diff_gp = cut(days_diff_f,
    breaks = c(
      -0.1,
      30, 60, 90, 120, 150, 180, 210,
      240, 270, 300, 330,
      seq(365, 7300, 365),
      19113
    ),
    right = T
  )) %>%
  select(date, total_asset_value, contractno, icu, prop_p, gia_p, non_gia_p, tdr_p, sv_p, stra_p, index_p, days_diff_f, days_diff_gp, discount)
dim(mydata_one_year_short_20)

tab_20 <- mydata_one_year_short_20 %>%
  group_by(days_diff_gp) %>%
  summarise(
    total = n(),
    "total_asset(B)" = sum(total_asset_value),
    prop_avg = round(mean(prop_p), 1),
    gia_avg = round(mean(gia_p), 1),
    non_gia_avg = round(mean(non_gia_p), 1),
    tdr_avg = round(mean(tdr_p), 1),
    sv_avg = round(mean(sv_p), 1),
    stra_avg = round(mean(stra_p), 1),
    index_avg = round(mean(index_p), 1)
  )


tab_20 %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)



#'
#' # Overall Prop Fund % Evolvement
#'
#'
#' prop fund summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = prop_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = prop_avg, x = days_diff_gp, y = prop_avg * 700), colour = "blue", vjust = -2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean Prop Fund (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'

#'
#' Prop Fund summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = prop_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = prop_avg, x = days_diff_gp, y = prop_avg * 700), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 1) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean Prop Fund (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g

#'
#' # Prop Fund: GIA and non-GIA
#'
#' ## GIA % Evolvement
#'
#'
#' GIA summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = gia_avg * 100)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = gia_avg, x = days_diff_gp, y = gia_avg * 100), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 100,
      name = "Mean GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'
#'
#' GIA summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = gia_avg * 600)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = gia_avg, x = days_diff_gp, y = gia_avg * 600), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = -2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 600,
      name = "Mean GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' ## non-GIA Prop % Evolvement
#'
#' non-GIA summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = non_gia_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = non_gia_avg, x = days_diff_gp, y = non_gia_avg * 700), colour = "blue", vjust = -2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean non-GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'
#' non-GIA summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = non_gia_avg * 2000)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = non_gia_avg, x = days_diff_gp, y = non_gia_avg * 2000), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 1) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean non-GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g



#'
#' # Prop Fund: Target Date, Stable Value, Strategy and Index
#'
#' ## Target Date % Evolvement
#'
#'
#' Target summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = tdr_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = tdr_avg, x = days_diff_gp, y = tdr_avg * 700), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean Target Date (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'
#'
#' Target summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = tdr_avg * 600)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = tdr_avg, x = days_diff_gp, y = tdr_avg * 600), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = -1) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 600,
      name = "Mean Target Date (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g

#'
#' ## Stable Value % Evolvement
#'
#'
#' Stable Value summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = sv_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = sv_avg, x = days_diff_gp, y = sv_avg * 700), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean Stable Value (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'
#'
#' Stable Value summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = sv_avg * 600)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = sv_avg, x = days_diff_gp, y = sv_avg * 600), colour = "blue", vjust = 1) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 1) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 600,
      name = "Mean Stable Value (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g



#'
#' ## Strategy % Evolvement
#'
#'
#' Strategy summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = stra_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = stra_avg, x = days_diff_gp, y = stra_avg * 700), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean Strategy (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'
#'
#' Strategy summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = stra_avg * 600)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = stra_avg, x = days_diff_gp, y = stra_avg * 600), colour = "blue", vjust = -2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = -1) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 600,
      name = "Mean Strategy (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g

#'
#' ## Index % Evolvement
#'
#'
#' Index summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = index_avg * 700)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = index_avg, x = days_diff_gp, y = index_avg * 700), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 700,
      name = "Mean Index (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * Put 20+ years as one group
#'
#' group policies whose years more than 20 years  (7300 days)
#'
#'
#' Index summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab_20, aes(x = days_diff_gp, y = index_avg * 600)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = index_avg, x = days_diff_gp, y = index_avg * 600), colour = "blue", vjust = 1) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = -2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 600,
      name = "Mean Index (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#' # Target Date Discount Impact Evolvement
#'
#'
#+ echo = F, results = 'hide'

tab_2 <- mydata_one_year_short_20 %>%
  filter(date %in% c("09/30/2019", "04/30/2020")) %>%
  group_by(days_diff_gp, date) %>%
  summarise(
    total = n(),
    "total_asset(B)" = sum(total_asset_value),
    prop_avg = round(mean(prop_p), 1),
    gia_avg = round(mean(gia_p), 1),
    non_gia_avg = round(mean(non_gia_p), 1),
    tdr_avg = round(mean(tdr_p), 1),
    sv_avg = round(mean(sv_p), 1),
    stra_avg = round(mean(stra_p), 1),
    index_avg = round(mean(index_p), 1)
  )

tab_2 %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)



tab_all <- mydata_one_year_short_20 %>%
  group_by(days_diff_gp, date) %>%
  summarise(
    total = n(),
    "total_asset(B)" = sum(total_asset_value),
    prop_avg = round(mean(prop_p), 1),
    gia_avg = round(mean(gia_p), 1),
    non_gia_avg = round(mean(non_gia_p), 1),
    tdr_avg = round(mean(tdr_p), 1),
    sv_avg = round(mean(sv_p), 1),
    stra_avg = round(mean(stra_p), 1),
    index_avg = round(mean(index_p), 1)
  ) %>%
  arrange(days_diff_gp, as.Date(as.character(date), format = "%m/%d/%Y"))


tab_all %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab_all$date <- as.Date(as.character(tab_all$date), format = "%m/%d/%Y")

#' ## Discount Impact Evolvement on Prop Fund
#'
#' * 09/30/2019 v.s. 04/30/2020
#'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = prop_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean Prop Fund (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = prop_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean Prop Fund (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' ## GIA and non-GIA
#'
#' ### Discount Impact Evolvement on GIA
#'
#' * 09/30/2019 v.s. 04/30/2020
#'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = gia_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g

#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = gia_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g

#'
#'
#' ### Discount Impact Evolvement on non-GIA prop
#'
#' * 09/30/2019 v.s. 04/30/2020
#'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = non_gia_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean non-GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = non_gia_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean non-GIA (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' ## Target Date, Stable Value, Strategy and Index
#'
#'
#' ### Discount Impact Evolvement on Target Date
#'
#' * 09/30/2019 v.s. 04/30/2020
#'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = tdr_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean Target Date (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'

#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = tdr_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean Target Date (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' ### Discount Impact Evolvement on Stable Value
#'
#' * 09/30/2019 v.s. 04/30/2020
#'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = sv_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean Stable Value (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'

#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = sv_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean Stable Value (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g

#'
#' ### Discount Impact Evolvement on Strategy and Risk
#'
#' * 09/30/2019 v.s. 04/30/2020
#'

#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = stra_avg * 200, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 200,
      name = "Mean Strategy (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = stra_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean Strategy (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g



#'
#' ### Discount Impact Evolvement on Index
#'
#' * 09/30/2019 v.s. 04/30/2020
#'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_2, aes(x = days_diff_gp, y = index_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean Index (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g


#'
#' * All months comparisons
#'
#+ echo = F, results = 'hide'


#'
#+ fig.width=12, echo = F

g <- ggplot(tab_all, aes(x = days_diff_gp, y = index_avg * 2000, color = date)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total, color = date), stat = "identity", fill = "tan1") +
  geom_point(size = 1) +
  geom_line(aes(group = date), size = 0.4) +
  #  geom_text(aes(label=tdr_avg, x=days_diff_gp, y=tdr_avg*200), colour="blue", vjust = 2)+
  #  geom_text(aes(label=total, x=days_diff_gp, y=total, color = date))+
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 2000,
      name = "Mean Index (%)",
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    )
  ) +
  xlab("Days Evolved") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90)
  )
g



#' From above, the Target Date Discount has positive impact
#' on Prop Fund overall. To be specific, it has positive impact
#' on TD fund and negative impact on Strategy and Index, where
#' Index fund is impacted more heavily than Strategy,
#' and almost no impact on the Stable Value.
#'
#+ echo = F


# test
#
# tab <- mydata_one_year_short %>%
#   mutate(gia_gp = case_when(gia_p<=0 ~ 0,
#                             gia_p==100 ~100,
#                             TRUE ~ 50)) %>%
#   group_by(days_diff_gp, gia_gp) %>%
#   summarise(total=n(),
#             gia_avg = round(mean(gia_p),1),
#             gia_md = median(gia_p))
#
# tab %>%
#   knitr::kable(format = "html") %>%
#   kableExtra::kable_styling(full_width = F)
