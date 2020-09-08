#' ---
#' title: QC based on 12-31-2019 data
#' author: Han Zhang
#' output:
#'    html_document:
#'      toc: true
#' ---

# --------  1. Data Load --------- #
#' ## Introduction
#'
#' In this report, I want to quality check of the prop capture data through 
#' its comparions with summary table based on the data collected at 12-31-2019.
#'
#' I will conduct the analysis in the following three parts. In the section [Data QC](#QC),
#' I will investigate the data source by itself through the unique key validation based on
#' the combination of contract number and ICU. And also the segment size feature creation against
#' the real contract total asset column. In section [Report QC](#summary), I will summarize the 
#' prop capture rates based on asset segment size group and Segments. 
#' Fianlly, the section [Taft Hartley QC](#TH) is used to make diagnostics on any
#' mis-match appeared in previous sections.
#'
#'
#+ echo = F
library(dplyr)
library(ggplot2)
my_data <- read.csv(
  file = "./data/cfr_12_31_2019.csv",
  na.strings = c("NA", "-", "", "."), header = TRUE, colClasses = c("character")
)

#' As needed by the project, we only focus on the data related to defined contribution,
#' investment only and non-qualified, and from either platform ReFlex, OMNI or TRAC.
#' The corresponding summaries are as below,
table(my_data$ProductType)
table(my_data$DCorDB)
table(my_data$ProductType, my_data$DCorDB, useNA = "ifany")
table(my_data$DCorDB, my_data$PlatformID, useNA = "ifany")

#+ echo = F
my_data <- my_data %>% filter(DCorDB == "DC", PlatformID %in% c("1", "2", "3"))
# --------  2. Data Quality Check --------- #
#' ## Data QC {#QC}
#+ echo = F, result = 'hide', include = F
# Basic summary and QC #
summary(as.numeric(my_data$Total.Proprietary.Assets..))
boxplot(as.numeric(my_data$Total.Proprietary.Assets..))
hist(as.numeric(my_data$Total.Proprietary.Assets..))

#' * Check Duplications of the Data
length(unique(my_data$ContractNo))
length(unique(paste(my_data$ContractNo, my_data$ICU, sep = "-")))
dim(my_data)[1]
#' The combination of contract number and ICU is the unique key.
#'
#' * Check if the Stable Value is Equal to GIA + SAGIC + CAPPRES.
my_data <- my_data %>%
  mutate(
    qc_stable_1 = as.numeric(GIA) + as.numeric(SAGIC) + as.numeric(CAPPRES),
    qc_stable_2 = as.numeric(TotalContractAssets) * as.numeric(X..of.MM.Stable.Value.Assets),
    qc_stable_c1 = between((qc_stable_1 + 1) / (qc_stable_2 + 1), 0.999, 1.001),
    qc_stable_c2 = qc_stable_1 == qc_stable_2,
    qc_stable_d = abs(qc_stable_2 - qc_stable_1),
    qc_stable_f = (qc_stable_1 + 1) / (qc_stable_2 + 1)
  )
table(my_data$qc_stable_c1)
#' There are ``r as.numeric(table(my_data$qc_stable_c1)[1])`` out of ``r dim(my_data)[1]``, only
#' ``r round(as.numeric(table(my_data$qc_stable_c1)[1])/dim(my_data)[1]*100,2)``\% of 
#' contracts that do not match exactly.
#'
#' Here are some summaries of those contracts that do not match exactly.
#'
#' The summary is based on the difference as below.
summary(my_data %>% filter(qc_stable_c1 != TRUE, qc_stable_c2 != TRUE) %>% select(qc_stable_d))
#' The summary is based on the fraction as below.
summary(my_data %>% filter(qc_stable_c1 != TRUE, qc_stable_c2 != TRUE) %>% select(qc_stable_f))
#' Although there are some mis-matches here, I will ignore them for now becuase of the small portion.

#+ echo = F
# summary(as.Date(my_data$EffectiveDate,"%m/%d/%y"))

# create response variable
my_data <- my_data %>% mutate(response = case_when(
  as.numeric(Total.Proprietary.Assets..) == 1 ~ 3,
  as.numeric(Total.Proprietary.Assets..) == 0 ~ 1,
  TRUE ~ 2
))

# look at segments with real asset
data_plot <- my_data %>%
  mutate(asset_size = case_when(
    Asset.Segment.Size == "$0 - $250K" ~ "a. $0 - $250K",
    Asset.Segment.Size == "$250K - $1M" ~ "b. $250K - $1M",
    Asset.Segment.Size == "$1 - $5M" ~ "c. $1 - $5M",
    Asset.Segment.Size == "$5M -$15M" ~ "d. $5M -$15M",
    Asset.Segment.Size == "$15M - $75M" ~ "e. $15M - $75M",
    Asset.Segment.Size == "$75M - $150M" ~ "f. $75M - $150M",
    Asset.Segment.Size == "$150M+" ~ "g. $150M+",
    TRUE ~ "h. Other"
  ))


#' * Check the Segment Size
tab <- data_plot %>%
  group_by(asset_size) %>%
  summarize(
    n = n(),
    "min(M)" = round(min(as.numeric(TotalContractAssets)) / 1000000, 2),
    "mean(M)" = round(mean(as.numeric(TotalContractAssets)) / 1000000, 2),
    "max(M)" = round(max(as.numeric(TotalContractAssets)) / 1000000, 2)
  )
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#' From above, one could see that the segment size feature is correct.

# --------  3. Summary --------- #
#' ## Report QC {#summary}
#' * Overal Prop Capture Rate Summary and GIA Rate
summary_data_prop <- my_data %>%
  ungroup() %>%
  summarise(
    total_asset_p = sum(as.numeric(TotalContractAssets)),
    total_plan = sum(as.numeric(Plan.Count)),
    "total_paricipant(M)" = round(sum(as.numeric(PptCount)) / 1000000, 3),
    prop_asset_p = sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)),
    prop_r = round(prop_asset_p / total_asset_p * 100),
    stable_p = sum(as.numeric(TotalContractAssets) * as.numeric(X..of.MM.Stable.Value.Assets)),
    stable_r = round(stable_p / prop_asset_p * 100),
    gia_sagic = sum(as.numeric(GIA), as.numeric(SAGIC)),
    gia_sagic_r = round(gia_sagic / prop_asset_p * 100),
    non_sta_p = prop_asset_p - stable_p
  )
summary_data_prop

#' * Prop Rate by Segment and Asset Segment Size

summary_data_prop <- my_data %>%
  mutate(
    asset_size = case_when(
      Asset.Segment.Size == "$0 - $250K" ~ "a. $0 - $250K",
      Asset.Segment.Size == "$250K - $1M" ~ "b. $250K - $1M",
      Asset.Segment.Size == "$1 - $5M" ~ "c. $1 - $5M",
      Asset.Segment.Size == "$5M -$15M" ~ "d. $5M -$15M",
      Asset.Segment.Size == "$15M - $75M" ~ "e. $15M - $75M",
      Asset.Segment.Size == "$75M - $150M" ~ "f. $75M - $150M",
      Asset.Segment.Size == "$150M+" ~ "g. $150M+",
      TRUE ~ "h. Other"
    ),
    segments = case_when(
      Segment %in% c("Corporate", "PEO") ~ "1. Corporate",
      Segment == "Government" ~ "2. Government",
      Segment == "Not For Profit" ~ "3. NFP",
      Segment == "Taft Hartley" ~ "4. Taft Hartley",
      TRUE ~ "5. Other"
    )
  ) %>%
  ungroup() %>%
  group_by(asset_size, segments) %>%
  summarise(
    total = n(),
    total_asset = sum(as.numeric(TotalContractAssets)),
    prop_asset = sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)),
    avg_prop = prop_asset / total_asset
  )

#+ echo = F, result = 'hide', include = F
summary_data_prop_wide <- reshape(data.frame(summary_data_prop[, c(1, 2, 6)]),
  direction = "wide", timevar = "asset_size",
  idvar = c("segments")
)
colnames(summary_data_prop_wide) <- gsub("avg_prop.", "", colnames(summary_data_prop_wide))
tab <- data.frame(summary_data_prop_wide)
tab[, 2:dim(tab)[2]] <- round(tab[, 2:dim(tab)[2]] * 100)
names(tab) <- names(summary_data_prop_wide)

#+ warning = F
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#' * Segment Only
summary_data_prop <- my_data %>%
  mutate(
    asset_size = case_when(
      Asset.Segment.Size == "$0 - $250K" ~ "a. $0 - $250K",
      Asset.Segment.Size == "$250K - $1M" ~ "b. $250K - $1M",
      Asset.Segment.Size == "$1 - $5M" ~ "c. $1 - $5M",
      Asset.Segment.Size == "$5M -$15M" ~ "d. $5M -$15M",
      Asset.Segment.Size == "$15M - $75M" ~ "e. $15M - $75M",
      Asset.Segment.Size == "$75M - $150M" ~ "f. $75M - $150M",
      Asset.Segment.Size == "$150M+" ~ "g. $150M+",
      TRUE ~ "h. Other"
    ),
    segments = case_when(
      Segment %in% c("Corporate", "PEO") ~ "1. Corporate",
      Segment == "Government" ~ "2. Government",
      Segment == "Not For Profit" ~ "3. NFP",
      Segment == "Taft Hartley" ~ "4. Taft Hartley",
      TRUE ~ paste("5.", Segment, sep = " ")
    )
  ) %>%
  ungroup() %>%
  group_by(segments) %>%
  summarise(
    "total_asset(B)" = round(sum(as.numeric(TotalContractAssets)) / 1000000000, 3),
    total_plan = n(),
    "total_paricipant(M)" = round(sum(as.numeric(PptCount)) / 1000000, 3),
    "prop_asset_p(B)" = round(sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)) / 1000000000, 3),
    "stable_p(B)" = round(sum(as.numeric(TotalContractAssets) * as.numeric(X..of.MM.Stable.Value.Assets)) / 1000000000, 3),
    "gia_p(B)" = sum(as.numeric(GIA)),
    avg_prop = round(sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)) /
      sum(as.numeric(TotalContractAssets)) * 100),
    avg_stable = round(sum(as.numeric(TotalContractAssets) * as.numeric(X..of.MM.Stable.Value.Assets)) /
      sum(as.numeric(TotalContractAssets)) * 100),
    gia_r = round(sum(as.numeric(GIA)) / sum(as.numeric(TotalContractAssets)) * 100),
    gia_p_r = round(sum(as.numeric(GIA)) / sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)) * 100)
  )

#+ warning = F
tab <- summary_data_prop
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#' * Asset Size Only
summary_data_prop <- my_data %>%
  mutate(
    asset_size = case_when(
      Asset.Segment.Size == "$0 - $250K" ~ "a. $0 - $250K",
      Asset.Segment.Size == "$250K - $1M" ~ "b. $250K - $1M",
      Asset.Segment.Size == "$1 - $5M" ~ "c. $1 - $5M",
      Asset.Segment.Size == "$5M -$15M" ~ "d. $5M -$15M",
      Asset.Segment.Size == "$15M - $75M" ~ "e. $15M - $75M",
      Asset.Segment.Size == "$75M - $150M" ~ "f. $75M - $150M",
      Asset.Segment.Size == "$150M+" ~ "g. $150M+",
      TRUE ~ "h. Other"
    ),
    segments = case_when(
      Segment == "Corporate" ~ "1. Corporate",
      Segment == "Government" ~ "2. Government",
      Segment == "Not For Profit" ~ "3. NFP",
      Segment == "Taft Hartley" ~ "4. Taft Hartley",
      TRUE ~ paste("5.", Segment, sep = " ")
    )
  ) %>%
  ungroup() %>%
  group_by(asset_size) %>%
  summarise(
    "total_asset(B)" = round(sum(as.numeric(TotalContractAssets)) / 1000000000, 3),
    total_plan = sum(as.numeric(Plan.Count)),
    "total_paricipant(M)" = round(sum(as.numeric(PptCount)) / 1000000, 3),
    "prop_asset_p(B)" = round(sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)) / 1000000000, 3),
    "stable_p(B)" = round(sum(as.numeric(TotalContractAssets) * as.numeric(X..of.MM.Stable.Value.Assets)) / 1000000000, 3),
    "gia_p(B)" = sum(as.numeric(GIA)),
    avg_prop = round(sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)) /
      sum(as.numeric(TotalContractAssets)) * 100),
    avg_stable = round(sum(as.numeric(TotalContractAssets) * as.numeric(X..of.MM.Stable.Value.Assets)) /
      sum(as.numeric(TotalContractAssets)) * 100),
    gia_r = round(sum(as.numeric(GIA)) / sum(as.numeric(TotalContractAssets)) * 100),
    gia_p_r = round(sum(as.numeric(GIA)) / sum(as.numeric(TotalContractAssets) * as.numeric(Total.Proprietary.Assets..)) * 100)
  )

#+ warning = F
tab <- summary_data_prop
tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#' ## Taft Hartley QC{#TH}
#'

tf_list <- read.csv(
  file = "./data/qc/cfr_12_31_2019_traft_qc.csv",
  na.strings = c("NA", "-", "", "."), header = TRUE, colClasses = c("character")
)
tf_list_heather <- unique(paste(tf_list$ContractNo, tf_list$ICU, sep = "-"))

tf_han_list <- my_data %>%
  filter(Segment == "Taft Hartley") %>%
  select(ContractNo, ICU, Plan.Count)
tf_list_han <- unique(paste(tf_han_list$ContractNo, tf_han_list$ICU, sep = "-"))

setdiff(tf_list_heather, tf_list_han)
setdiff(tf_list_han, tf_list_heather)

#' My list matches to Heather's.
#'
#'
#' ## Conclusion
#'
#'
#' As a conclusion, we have almost the same result as what Heather and Chris
#' provide, and we decide to move forward.
