#' ---
#' title: Prop Capture Model
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
library(mgcv)
library("reshape2")
# self defined functions
source("./src/ead_src.R")
source("./src/ae_src.R")
source("./src/lift_src.R")
source("./src/ppv_src.R")

# --------  1. Data Load --------- #
#' # Introduction
#'
#' In this report, I will focus on modeling. Specifically, I will merge Advisor
#' data to policy data.
#'
#'
#+ echo = F, warning = F, results = 'hide'

# load advisor feature data
load("./data/advisor_pol.rda")
load("./data/advisor_firm_pol.rda")
# mydata_adv_pol

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
  "Total Proprietary Assets %",
  "TPA",
  "Overall EBA & PERA $",
  "Overall Net Comm $"
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
Reduce(identicalValue, col_names_cft_one_yr) 
# if print the column names, then it indicates they are all have the same col. names

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

# only keep the latest one

mydata_one_year_last <- mydata_one_year %>%
  mutate(
    days_diff = as.Date(as.character(date), format = "%m/%d/%Y") -
      as.Date(as.character(effectivedate), format = "%m/%d/%Y"),
    days_diff_f = case_when(
      as.numeric(days_diff) < 0 ~ 0,
      TRUE ~ as.numeric(days_diff)
    )
  ) %>%
  filter(!is.na(days_diff_f)) %>%
  arrange(contractno, icu, desc(days_diff_f)) %>%
  group_by(contractno, icu) %>%
  filter(row_number() == 1)

dim(mydata_one_year)
dim(mydata_one_year_last)

# qc: make sure we keep the most current observations by excel
tab <- mydata_one_year_last %>%
  group_by(date) %>%
  summarise(total = n()) %>%
  arrange(as.Date(as.character(date), format = "%m/%d/%Y"))
print(tab)

# View(mydata_one_year_last %>% filter(date == '09/30/2019'))
# View(mydata_one_year_last %>% filter(date == '12/31/2019'))
# View(mydata_one_year_last %>% filter(date == '01/31/2020'))
# View(mydata_one_year_last %>% filter(date == '02/29/2020'))


#'
#' Almost all policies come from the 04/30/2020 file, the most current one, around
#' 93.8%. For the rest, roughly less than 1% of the policy come from each of other
#' files. And I have already checked with the original data source to make sure we
#' indeed get the most recent policies.
#'
#+ echo = F, warning = F


# qc: make sure contract and icu is the unique identification number
# dim(mydata_one_year_last)
# length(unique(paste(mydata_one_year_last$icu,mydata_one_year_last$contractno)))
# only use the most recent data

table(mydata_one_year_last$date)
data_recent <- mydata_one_year_last %>%
  filter(date == "04/30/2020")
table(data_recent$date)

#'
#' I will focus on those 24,777 polices that are in 2020-04-30 file to build our model.
#'
#+ echo = F, warning = F

# length(unique(paste(data_recent$contractno,data_recent$icu)))
# merge advisor features data with our policy data

data_recent_model <- data_recent %>%
  mutate(key = paste(contractno, icu, sep = "-")) %>%
  left_join(mydata_adv_pol, by = "key") %>%
  left_join(mydata_firm_v6, by = "key") %>%
  mutate(keyaccount = case_when(
    is.na(key_account_firm) ~ "No-Key-Account-Firm or NA",
    TRUE ~ key_account_firm
  ))


# dim(data_recent_model)
tab <- table(data_recent_model$total_plan_p, useNA = "ifany")


# some modifications of data colnumn names
data_recent_model <- data_recent_model %>%
  mutate(prop_r_curr = as.numeric(total_proprietary_assets__))




#'
#' # Sanity Check
#'
#+ echo = F, warning = F


# create days difference group



dim(data_recent_model)
mydata_one_year <- data_recent_model %>%
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
  select(
    date,
    contractno,
    icu, totalcontractassets,
    total_asset_value, contractno,
    icu, prop_p, gia_p, non_gia_p, tdr_p, sv_p, stra_p, index_p, days_diff_f, days_diff_gp, discount
  )
dim(mydata_one_year_short)



tab <- mydata_one_year_short %>%
  group_by(days_diff_gp) %>%
  summarise(
    total = n(),
    prop_zero_gp = sum(prop_p == 0),
    prop_zero_pt = round(prop_zero_gp / total, 3) * 100,
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
  select(date, total_asset_value, 
         contractno, icu, prop_p, 
         gia_p, non_gia_p, tdr_p, 
         sv_p, stra_p, index_p, 
         days_diff_f, days_diff_gp, 
         discount)
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
#' ## Overall Prop Fund % Evolvement
#'
#'
#' prop fund summary by days evolving
#'
#+ fig.width=12, echo = F

g <- ggplot(tab, aes(x = days_diff_gp, y = prop_avg * 35)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = prop_avg, x = days_diff_gp, y = prop_avg * 35), colour = "blue", vjust = -2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 2) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 35,
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

g <- ggplot(tab_20, aes(x = days_diff_gp, y = prop_avg * 35)) +
  geom_bar(mapping = aes(x = days_diff_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  geom_point(size = 1, color = "blue") +
  geom_line(aes(group = 1), size = 0.4, color = "blue") +
  geom_text(aes(label = prop_avg, x = days_diff_gp, y = prop_avg * 35), colour = "blue", vjust = 2) +
  geom_text(aes(label = total, x = days_diff_gp, y = total), colour = "black", vjust = 1) +
  scale_y_continuous(
    name = "Total Number of Policies",
    sec.axis = sec_axis(~ . / 35,
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

#' Above, it looks very similar to the summary plots when using all 8 months data, which
#' provides confidence for us to proceed our analysis with this data.

#'
#' # Feature Engineering
#'
#'
#'
# Model cycling starts from here
data_recent_model_final <- data_recent_model %>%
  ungroup() %>%
  mutate(
    response = prop_r_curr,
    response_m = case_when(
      prop_r_curr <= 0.0001 ~ 0.0001,
      prop_r_curr >= 0.99 ~ 0.99,
      TRUE ~ prop_r_curr
    ),
    response_m_1 = as.numeric(cut(response_m,
      breaks = seq(0.0001, 0.99, 0.0001),
      include.lowest = T
    )),
    response_model = response_m_1 / 10000,
    asset_size = case_when(
      asset_segment_size == "$0 - $250K" ~ "a. $0 - $250K",
      asset_segment_size == "$250K - $1M" ~ "b. $250K - $1M",
      asset_segment_size == "$1 - $5M" ~ "c. $1 - $5M",
      asset_segment_size == "$5M -$15M" ~ "d. $5M -$15M",
      asset_segment_size == "$15M - $75M" ~ "e. $15M - $75M",
      asset_segment_size == "$75M - $150M" ~ "f. $75M - $150M",
      asset_segment_size == "$150M+" ~ "g. $150M+",
      TRUE ~ "h. Other"
    ),
    totalcontractassets_m = as.numeric(totalcontractassets) / 1000000,
    total_asset_gp = cut(totalcontractassets_m,
      breaks = unique(quantile(totalcontractassets_m,
        probs = seq(0, 1, 0.05)
      )),
      include.lowest = T
    ),
    total_asset_f = as.numeric(total_asset_gp),
    total_asset_gp_v1 = cut(totalcontractassets_m,
      breaks = unique(quantile(totalcontractassets_m, probs = seq(0, 1, 0.1))),
      include.lowest = T
    ),
    total_asset_f_v1 = as.numeric(total_asset_gp_v1),
    total_asset_f_v2 = case_when(
      as.numeric(total_asset_f) <= 8 ~ 8,
      TRUE ~ as.numeric(total_asset_f)
    ),


    tenure_y = ceiling(as.numeric(days_diff_f) / 365.5),
    tenure = case_when(
      tenure_y >= 20 ~ 20,
      tenure_y == 0 ~ 1,
      TRUE ~ tenure_y
    ),
    industry = case_when(
      segment %in% c("Corporate", "Not For Profit", "PEO") ~ "1. CNP",
      TRUE ~ "2. GT"
    ),
    industry_v1 = case_when(
      segment %in% c("Corporate", "Not For Profit", "PEO") ~ "1. CNP",
      segment %in% c("Government") ~ "2. Gv",
      TRUE ~ "3. Ta"
    ),
    industry_v2 = case_when(
      segment %in% c("Government") ~ "2. Gv",
      TRUE ~ "1. CNPT"
    ),
    eba_comm = case_when(
      as.numeric(overall_eba___pera__) +
        as.numeric(overall_net_comm__) < 5 ~ 0,
      TRUE ~ 1
    ),
    total_plan_p_v0 = case_when(
      is.na(total_plan_p) & is.na(tpa) & eba_comm == 0 ~ 22,
      is.na(total_plan_p) & tpa == "UNASSIGNED" & eba_comm == 0 ~ 22,
      is.na(total_plan_p) & is.na(tpa) & eba_comm == 1 ~ 24,
      is.na(total_plan_p) & tpa == "UNASSIGNED" & eba_comm == 1 ~ 24,
      is.na(total_plan_p) & !is.na(tpa) & eba_comm == 0 ~ 26,
      is.na(total_plan_p) & !is.na(tpa) & eba_comm == 1 ~ 28,
      total_plan_p >= 20 ~ 20,
      TRUE ~ as.double(total_plan_p)
    ),
    total_plan_p_v1 = case_when(
      is.na(total_plan_p) & is.na(tpa) & eba_comm == 1 ~ 0,
      is.na(total_plan_p) & tpa == "UNASSIGNED" & eba_comm == 1 ~ 0,
      is.na(total_plan_p) & !is.na(tpa) & eba_comm == 0 ~ 0,
      is.na(total_plan_p) & !is.na(tpa) & eba_comm == 1 ~ 0,
      is.na(total_plan_p) ~ 20,
      total_plan_p >= 20 ~ 20,
      TRUE ~ as.double(total_plan_p)
    ),
    total_plan_p_v2 = case_when(
      is.na(total_plan_p) & is.na(tpa) & eba_comm == 1 ~ 0,
      is.na(total_plan_p) & tpa == "UNASSIGNED" & eba_comm == 1 ~ 0,
      is.na(total_plan_p) & !is.na(tpa) & eba_comm == 0 ~ 0,
      is.na(total_plan_p) & !is.na(tpa) & eba_comm == 1 ~ 0,
      is.na(total_plan_p) ~ 3,
      total_plan_p >= 3 ~ 3,
      TRUE ~ as.double(total_plan_p)
    ),
    key_account_v1 = case_when(
      keyaccount == "MASSMUTUAL" ~ "1. MassMutual",
      keyaccount == "No-Key-Account-Firm or NA" ~ "3. No-Key-Account-Firm or NA",
      TRUE ~ "2. Other Key Account Firm Holder"
    )
  )
data_recent_model_final$industry_v1 <- as.factor(data_recent_model_final$industry_v1)
data_recent_model_final$industry <- as.factor(data_recent_model_final$industry)
data_recent_model_final$key_account_v1 <- as.factor(data_recent_model_final$key_account_v1)

save(data_recent_model_final, file = "./data/join_data.rda")

#'
#' # Data Split
#'
#+ echo = F
# load("./data/join_data.rda")
sample_size <- floor(0.8 * nrow(data_recent_model_final))
set.seed(498453)
train_picked <- sample(seq_len(nrow(data_recent_model_final)), size = sample_size)


train <- data_recent_model_final[train_picked, ]
test <- data_recent_model_final[-train_picked, ]

save(train, file = "./data/train_data.rda") # used for other ML methods input
load("./data/train_data.rda")

# make response based on (y*(n-1)+0.5)/n
# train$prop_r_curr <- (train$prop_r_curr*(dim(train)[1]-1)+0.5)/dim(train)[1]
# train$response <- (train$prop_r_curr*(dim(train)[1]-1)+0.5)/dim(train)[1]
train <- train %>%
  mutate(response_mod = log(prop_r_curr / (1 - prop_r_curr)))
hist(train$response_mod, breaks = 20)
# this is not enough as after tranferring, there are still many super small
# and large values
# make the reponses into 100 groups


# look at the spread of our response
train <- train %>%
  mutate(response_mod = log(response_model / (1 - response_model)))
hist(train$response_mod, breaks = 20)



#' I use the truncated response to avoid some extreme outliers when model fitting
#'
#' # EDA
#'
#'
#' Original Data without Groupings
#'
#'
#' ## number of policies advisor previously own
#'
#+ echo = F, warning = F, message = F

# hist(data_recent_model_final$total_plan_p)
# table(data_recent_model_final$total_plan_p,useNA='ifany')
data_recent_model_final_na <- train %>%
  filter(is.na(total_plan_p)) %>%
  select(contractno, icu, tpa, overall_eba___pera__, overall_net_comm__) %>%
  mutate(
    tpa_ext = case_when(
      is.na(tpa) ~ 0,
      tpa == "UNASSIGNED" ~ 0,
      TRUE ~ 1
    ),
    eba_comm = case_when(
      as.numeric(overall_eba___pera__) +
        as.numeric(overall_net_comm__) < 5 ~ 0,
      TRUE ~ 1
    )
  )

# table(data_recent_model_final_na$tpa,useNA = 'ifany')
# sum(is.na(data_recent_model_final_na$tpa))/dim(data_recent_model_final_na)[1]
# summary(as.numeric(data_recent_model_final_na$overall_eba___pera__),useNA = 'ifany')
# summary(as.numeric(data_recent_model_final_na$overall_net_comm__),useNA = 'ifany')


table(data_recent_model_final_na$tpa_ext)
table(data_recent_model_final_na$eba_comm)

# qc
# View(data_recent_model_final_na %>% filter(tpa_ext == 1, eba_comm == 1))

#'
#' There are 3,722 policies that do not have advisor features, occupying around 15.0 %
#' of data.
#'
#' Among those 3,722 policies, around 72.2% of them does not have advisors,
#' and around 65.2% does not have eba or commission fees more than $5.
#'
#'
#+ echo = F, warning = F, message = F

# View(train%>% filter(is.na(total_plan_p))%>%select(contractno,icu,total_plan_p))

#'
#+ echo = F, warning = F, message = F, fig.width=12

a <- ead_analysis(
  dat = train,
  smooth_ind = 1,
  smooth_ind_ori = 1,
  response_var = sym("response_model"),
  var = sym("total_plan_p"),
  title = "# of Policies Owned",
  vjust_m = -1
)
a$plot_ori
a$plot
#'
#'
#'
#' The loess plot is a locally smoothed line, which is a kind of EDA
#' analysis like histogram, it tries to caputre the pattern of the data
#' with consideration of local information like smoothing window approach.
#' Whereas, the lm is a simple linear line, which fit a linear regerssion with
#' number of data points as natural weight unlike loess.
#'
#'
#' Grouping policies with more than 20 policies into
#' the group of advisor with 20 policies. And, policies
#' with NA total plan feature will be split into 4 subgourps further:
#' policies with no tpa and commission fees less than $5 is in group 22,
#' policies with no tpa and commission fees more than $5 is in group 24,
#' policies with tpa and commission fees less than $5 is in group 26,
#' policies with tpa and commission fees more than $5 is in group 28.
#'
#'
#+ echo = F, warning = F, message = F, fig.width=12
a <- ead_analysis(
  dat = train,
  smooth_ind = 1,
  smooth_ind_ori = 1,
  response_var = sym("response_model"),
  var = sym("total_plan_p_v0"),
  title = "# of Policies Owned",
  vjust_m = -1
)
a$plot_ori
a$plot

#'
#'
#' From above plot, one could see that number of plicies previsouly owned by advisor become
#' sparse as the number is higher than 3 with 20 and 22 as the grouped ones.
#'
#' Also, for those policies that do not have advisor feature, policies who do not have tpa and
#' no commission fees there have very high prop capture rate, 46%, and policies who
#' has missing advisors have relative low prop capture rate, around 27%.
#'
#' Next, I will group advisors with more than 3 policies and no advisor at all into
#' the group of advisor with 3 policies, policies who has missing advisor features but
#' has missing advisor info in generall will be put into the 0 group.
#'
#'
#+ echo = F, warning = F, message = F, fig.width=12
a <- ead_analysis(
  dat = train,
  smooth_ind = 1,
  smooth_ind_ori = 1,
  response_var = sym("response_model"),
  var = sym("total_plan_p_v2"),
  title = "# of Policies Owned",
  vjust_m = 1
)
# a$plot_ori
a$plot

#'
#' From above, a linear trend looks approproiate here.
#'
#'
#' Next, as we explore more advisor features, I will add tenure, asset size and segment as well.
#' And, I will focus on the A/E analysis or fit analysis.
#'
#+ echo = F, warning = F, message = F, fig.width=12


#'
#' ## segment
#'
#'
#+ echo = F, warning = F, message = F, fig.width=12
# sum(is.na(train$segment)) # none is missing

a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 0,
  response_var = sym("response_model"),
  var = sym("segment"),
  title = "segment",
  vjust_m = 1
)
a$plot_ori
a$plot



#'
#' I will put Corporate, NFP and PEO as one group and Government and Taft Hartley as another group
#'
#+ echo = F, warning = F, message = F, fig.width=12
a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 0,
  response_var = sym("response_model"),
  var = sym("industry"),
  title = "segment",
  vjust_m = 1
)
# a$plot_ori
a$plot

#'
#' ## Asset Size
#'
#+ echo = F, warning = F, message = F, fig.width=12
a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 0,
  response_var = sym("response_model"),
  var = sym("total_asset_gp"),
  title = "Asset Size",
  vjust_m = 1
)
a$plot_ori

#'
#' Data is skewed to the right, I will group them by percentile.
#'
#+ echo = F, warning = F, message = F, fig.width=12

# group total assets by percentile
a$table
a$plot
#'
#' ## Tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12
a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 1,
  response_var = sym("response_model"),
  var = sym("tenure_y"),
  title = "tenure",
  vjust_m = 1
)
a$plot_ori

#'
#' Combined with what we have seen before I will put policies that are more than 20 years with us
#' as 20 year.
#'
#+ echo = F, warning = F, message = F, fig.width=12
# group tenure by percentile
a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 1,
  response_var = sym("response_model"),
  var = sym("tenure"),
  title = "tenure",
  vjust_m = 1,
  vjust_r = 1
)
a$plot_ori
# a$table
a$plot


#' ## key account
#'
#+ echo = F, warning = F, message = F, fig.width=14
a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 1,
  response_var = sym("response_model"),
  var = sym("keyaccount"),
  title = "Key Account",
  vjust_m = -1,
  vjust_r = -1
)
a$plot_ori
# a$table
a$plot


#'
#' I will treat MassMutual as one group, "non key account" as one group,
#' and the rest as another group.
#'
#+ echo = F, warning = F, message = F, fig.width=12
a <- ead_analysis(
  dat = train,
  smooth_ind = 0,
  smooth_ind_ori = 0,
  response_var = sym("response_model"),
  var = sym("key_account_v1"),
  title = "Key Account Type",
  vjust_m = 1
)
# a$plot_ori
a$plot





#'
#'
#' # Model Building
#'
#'
#' I will build a simple model to start. For the AE analysis here, it is used for diagnostics. The
#' group wise version tends to be more concervatives. I will use both groupwise and individual
#' version of them, and the plot is based on the individual one.
#'
#'
#' ## Model with Normality Assumption
#'
#+ echo = F, warning = F, message = F, fig.width=12

# see how many cores that we have
parallel::detectCores()


train_model_v0 <- gam(response_model ~ 1,
  method = "ML",
  data = train,
  family = gaussian(link = "identity"),
  control = gam.control(
    nthreads = 1,
    trace = F,
    maxit = 50
  )
)

summary(train_model_v0)
par(mfrow = c(2, 2))
gam.check(train_model_v0,
  type = "deviance",
  pch = 19,
  cex = .1
)

#'
#' Residuals are skewed to the right with Gaussian error assumptions
#' use beta regression
#'


#'
#' ## Model with Beta Assumption
#'
#'
#' ### intercept model
#'
#+ echo = F, warning = F, message = F, fig.width=12
train_model_v1 <- gam(response_model ~ 1,
  method = "fREML",
  data = train,
  family = betar(link = "logit"),
  control = gam.control(
    nthreads = 1,
    trace = F,
    maxit = 50
  ),
  gamma = 1.5,
  select = T
)

summary(train_model_v1)
par(mfrow = c(2, 2))
gam.check(train_model_v1,
  type = "deviance",
  pch = 19,
  cex = .1
)

#'
#' * key account
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v1,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("keyaccount"),
  var_des = "Key Account (Original)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

ae_analysis(
  model = train_model_v1,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("key_account_v1"),
  var_des = "Key Account (Model)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

#'
#' * industry
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v1,
  dat_train = train,
  ymin = 0,
  ymax = 17000,
  var = sym("industry_v1"),
  var_des = "Segment"
)
#'
#' * total number of policies owned by advisor
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v1,
  dat_train = train,
  ymin = 0,
  ymax = 15000,
  var = sym("total_plan_p_v1"),
  var_des = "Total Number of Policies Owned Previously"
)
#'
#' * total asset
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v1,
  dat_train = train,
  ymin = 0,
  ymax = 8000,
  var = sym("total_asset_f_v2"),
  var_des = "Total Asset",
  scale = 5000
)
#'
#' * tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v1,
  dat_train = train,
  ymin = 0,
  ymax = 10000,
  var = sym("tenure"),
  var_des = "Tenure",
  scale = 5000
)
#' Model performs better than the normality assumption model,
#' some predictors should be considered for it shows lack of
#' fit in the A/E analsyis above.
#'
#' ### Total policies owned by advisor, segment, total asset, tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12
train_model_v2 <- gam(response_model ~
# s(key_account_v1,bs="re") +
# key_account_v1+
s(industry, bs = "re") +
  # industry_v1 +
  s(total_asset_f_v2, bs = "cr", k = 3) +
  s(total_plan_p_v1, bs = "cr", k = 3) +
  s(tenure, bs = "cr", k = 4),
method = "fREML",
data = train,
family = betar(link = "logit"),
# family = gaussian(link = "identity"),
control = gam.control(
  nthreads = 1,
  trace = F,
  maxit = 50
),
gamma = 1.5,
select = T
)
plot.gam(train_model_v2,
  rug = F,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2,
  residuals = F,
  pages = 1,
  trans = function(x) exp(x) / (1 + exp(x)),
  seWithMean = T
)
# train_model_v1
summary(train_model_v2)
par(mfrow = c(2, 2))
gam.check(train_model_v2,
  type = "deviance",
  pch = 19,
  cex = .1
)
#'
#' * key account
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v2,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("keyaccount"),
  var_des = "Key Account (Original)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

ae_analysis(
  model = train_model_v2,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("key_account_v1"),
  var_des = "Key Account (Model)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

#'
#' * industry
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v2,
  dat_train = train,
  ymin = 0,
  ymax = 17000,
  var = sym("industry_v1"),
  var_des = "Segment"
)
#'
#' * total number of policies owned by advisor
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v2,
  dat_train = train,
  ymin = 0,
  ymax = 15000,
  var = sym("total_plan_p_v1"),
  var_des = "Total Number of Policies Owned Previously"
)
#'
#' * total asset
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v2,
  dat_train = train,
  ymin = 0,
  ymax = 8000,
  var = sym("total_asset_f_v2"),
  var_des = "Total Asset",
  scale = 5000
)
#'
#' * tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v2,
  dat_train = train,
  ymin = 0,
  ymax = 10000,
  var = sym("tenure"),
  var_des = "Tenure",
  scale = 5000
)
#'
#' ### total policies owned by advisor, segment modified, total asset, tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12
# Start the clock!
ptm <- proc.time()
train_model_v3 <- gam(response_model ~
# s(key_account_v1,bs="re") +
# key_account_v1+
industry_v1 +
  s(total_asset_f_v2, bs = "cr", k = 3) +
  s(total_plan_p_v1, bs = "cr", k = 3) +
  s(tenure, bs = "cr", k = 4),
method = "fREML",
data = train,
family = betar(link = "logit"),
# family = gaussian(link = "identity"),
control = gam.control(
  nthreads = 1,
  trace = F,
  maxit = 50
),
gamma = 1.5,
select = T
)
# Stop the clock
proc.time() - ptm
# train_model_v2
summary(train_model_v3)
par(mfrow = c(2, 2))
gam.check(train_model_v3,
  type = "deviance",
  pch = 19,
  cex = .1
)
#'
#' * key account
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v3,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("keyaccount"),
  var_des = "Key Account (Original)",
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

ae_analysis(
  model = train_model_v3,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("key_account_v1"),
  var_des = "Key Account (Model)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

#'
#' * industry
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v3,
  dat_train = train,
  ymin = 0,
  ymax = 17000,
  var = sym("industry_v1"),
  var_des = "Segment"
)
#'
#' * total number of policies owned by advisor
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v3,
  dat_train = train,
  ymin = 0,
  ymax = 15000,
  var = sym("total_plan_p_v1"),
  var_des = "Total Number of Policies Owned Previously"
)
#'
#' * total asset
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v3,
  dat_train = train,
  ymin = 0,
  ymax = 8000,
  var = sym("total_asset_f_v2"),
  var_des = "Total Asset",
  scale = 5000
)
#'
#' * tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v3,
  dat_train = train,
  ymin = 0,
  ymax = 10000,
  var = sym("tenure"),
  scale = 5000
)
plot.gam(train_model_v3,
  rug = F,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2,
  residuals = F,
  pages = 1,
  trans = function(x) exp(x) / (1 + exp(x)),
  seWithMean = T
)
# why Ta has negative value
train %>%
  group_by(industry_v1) %>%
  summarise(
    mean_asset = mean(total_asset_f),
    mean_tenure = mean(tenure),
    mean_total_plan = mean(total_plan_p_v1)
  ) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#'
#' Ta has negave impact and it is not significant. It is because it is confounded with totol number of policies'
#' feature. That is, Ta tends to have advisors whose number of policies owned are large. Given above, we may
#' still group Ta and Gv together.
#'



#' ### Total policies owned by advisor, key account, segment, total asset, tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12
train_model_v4 <- gam(response_model ~
s(key_account_v1, bs = "re") +
  # key_account_v1 +
  s(industry, bs = "re") +
  # industry_v1 +
  s(total_asset_f_v2, bs = "cr", k = 3) +
  s(total_plan_p_v1, bs = "cr", k = 3) +
  s(tenure, bs = "cr", k = 4),
method = "fREML",
data = train,
family = betar(link = "logit"),
# family = gaussian(link = "identity"),
control = gam.control(
  nthreads = 1,
  trace = F,
  maxit = 50
),
gamma = 1.5,
select = T
)
plot.gam(train_model_v4,
  rug = F,
  all.terms = TRUE,
  shade = TRUE,
  shade.col = 2,
  residuals = F,
  pages = 1,
  trans = function(x) exp(x) / (1 + exp(x)),
  seWithMean = T
)
# train_model_v1
summary(train_model_v4)
par(mfrow = c(2, 2))
gam.check(train_model_v4,
  type = "deviance",
  pch = 19,
  cex = .1
)


#'
#' * key account
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v4,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("keyaccount"),
  var_des = "Key Account (Original)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)

ae_analysis(
  model = train_model_v4,
  dat_train = train,
  ymin = 0,
  ymax = 12000,
  var = sym("key_account_v1"),
  var_des = "Key Account (Model)",
  train = 1,
  scale = 5000,
  vjust_r = -1,
  vjust_m = -1
)
#'
#' * industry
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v4,
  dat_train = train,
  ymin = 0,
  ymax = 17000,
  var = sym("industry_v1"),
  var_des = "Segment"
)
#'
#' * total number of policies owned by advisor
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v4,
  dat_train = train,
  ymin = 0,
  ymax = 15000,
  var = sym("total_plan_p_v1"),
  var_des = "Total Number of Policies Owned Previously"
)
#'
#' * total asset
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v4,
  dat_train = train,
  ymin = 0,
  ymax = 8000,
  var = sym("total_asset_f_v2"),
  var_des = "Total Asset",
  scale = 5000
)
#'
#' * tenure
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
ae_analysis(
  model = train_model_v4,
  dat_train = train,
  ymin = 0,
  ymax = 10000,
  var = sym("tenure"),
  var_des = "Tenure",
  scale = 5000
)


#' # Model Performance
#'
#' ## Total policies owned by advisor, industry, total asset, tenure
#'
#' ### Actual v.s. Expected (Fitting on Original Features on Testing)
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
for (group in sort(unique(test$segment))) {
  dat <- test %>%
    filter(segment == group)
  ae_analysis(
    model = train_model_v2,
    dat_train = dat,
    ymin = 0,
    ymax = 1500,
    var = sym("asset_size"),
    var_des = paste("Asset Size (segment: ", group, ")", sep = ""),
    train_ind = 0,
    scale = 700
  )
}
#' ### PPV plot (Prediction Utility in Practice on Testing)
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
a <- ppv_summary(test,
  model = train_model_v2,
  gp_des = "All",
  num_group = 10,
  ref_pos = 5
)
print(a$table)
print(a$plot)
for (group in sort(unique(test$tenure))) {
  # print(group)
  dat <- test %>%
    filter(tenure == group)
  if (dim(dat)[1] > 500) {
    num_g <- 10
    ref_pos_g <- 5
  } else {
    num_g <- 5
    ref_pos_g <- 2
  }
  a <- ppv_summary(dat,
    model = train_model_v2,
    gp_des = paste(group, ", volume: ",
      dim(dat)[1], ", ptg: ",
      round(dim(dat)[1] / dim(test)[1], 3) * 100,
      "%",
      sep = ""
    ),
    num_group = num_g,
    ref_pos = ref_pos_g
  )
  print(a$table)
  print(a$plot)
}

#'
#' ### Lift Charts (Model Comparisons on Testing)
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
a <- lift_chart(test,
  model = train_model_v2,
  num_g = 10
)
a$tab
a$plot
acc_score <- numeric(10)
i <- 1
for (group in sort(unique(test$tenure))) {
  print(group)
  dat <- test %>%
    filter(tenure == group)
  if (dim(dat)[1] > 500) {
    num_g <- 10
  } else {
    num_g <- 5
  }
  a <- lift_chart(dat,
    model = train_model_v2,
    gp_des = paste(group, ", volume: ",
      dim(dat)[1], ", ptg: ",
      round(dim(dat)[1] / dim(test)[1], 3) * 100,
      "%",
      sep = ""
    ),
    num_group = num_g
  )
  print(a$tab)
  print(a$plot)
  acc_score[i] <- a$acc_score
  i <- i + 1
}

#'
#' The area under the lift curve overall of this model is `r a$acc_score`.
#' The area under the lift curve overall after controlling policy tenureis
#' `r sum(acc_score)`.
#'
#' ## Total policies owned by advisor, industry modified, total asset, tenure
#'
#' ### Actual v.s. Expected (Fitting on Original Features on Testing)
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
for (group in sort(unique(test$segment))) {
  dat <- test %>%
    filter(segment == group)
  ae_analysis(
    model = train_model_v3,
    dat_train = test,
    ymin = 0,
    ymax = 2000,
    var = sym("asset_size"),
    var_des = paste("Asset Size (segment: ", group, ")", sep = ""),
    train_ind = 0,
    scale = 1000
  )
}
#'
#' ### PPV plot (Prediction Utility in Practice on Testing)
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
a <- ppv_summary(test,
  model = train_model_v3,
  gp_des = "All",
  num_group = 10,
  ref_pos = 5
)
print(a$table)
print(a$plot)
for (group in sort(unique(test$tenure))) {
  # print(group)
  dat <- test %>%
    filter(tenure == group)
  if (dim(dat)[1] > 500) {
    num_g <- 5
    ref_pos_g <- 2
  } else {
    num_g <- 5
    ref_pos_g <- 2
  }
  a <- ppv_summary(dat,
    model = train_model_v3,
    gp_des = paste(group, ", volume: ",
      dim(dat)[1], ", ptg: ",
      round(dim(dat)[1] / dim(test)[1], 3) * 100,
      "%",
      sep = ""
    ),
    num_group = num_g,
    ref_pos = ref_pos_g
  )
  print(a$table)
  print(a$plot)
}

#'
#' ### Lift Charts (Model Comparisons on Testing)
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
a <- lift_chart(test,
  model = train_model_v3,
  num_g = 10
)
a$tab
a$plot
acc_score <- numeric(10)
i <- 1
for (group in sort(unique(test$tenure))) {
  print(group)
  dat <- test %>%
    filter(tenure == group)
  if (dim(dat)[1] > 500) {
    num_g <- 5
  } else {
    num_g <- 5
  }
  a <- lift_chart(dat,
    model = train_model_v3,
    gp_des = paste(group, ", volume: ",
      dim(dat)[1], ", ptg: ",
      round(dim(dat)[1] / dim(test)[1], 3) * 100,
      "%",
      sep = ""
    ),
    num_group = num_g
  )
  print(a$tab)
  print(a$plot)
  acc_score[i] <- a$acc_score
  i <- i + 1
}

#'
#' The area under the lift curve overall of this model is `r a$acc_score`.
#'
#' ## Total policies owned by advisor, key account, segment, total asset, tenure
#'
#' ### Actual v.s. Expected (Fitting on Original Features on Testing)
#'
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
for (group in sort(unique(test$segment))) {
  dat <- test %>%
    filter(segment == group)
  ae_analysis(
    model = train_model_v4,
    dat_train = dat,
    ymin = 0,
    ymax = 1500,
    var = sym("asset_size"),
    var_des = paste("Asset Size (segment: ", group, ")", sep = ""),
    train_ind = 0,
    scale = 700
  )
}
#'
#' ### PPV plot (Prediction Utility in Practice on Testing)
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
a <- ppv_summary(test,
  model = train_model_v4,
  gp_des = "All",
  num_group = 10,
  ref_pos = 5
)
print(a$table)
print(a$plot)
for (group in sort(unique(test$tenure))) {
  # print(group)
  dat <- test %>%
    filter(tenure == group)
  if (dim(dat)[1] > 500) {
    num_g <- 10
    ref_pos_g <- 5
  } else {
    num_g <- 5
    ref_pos_g <- 2
  }
  a <- ppv_summary(dat,
    model = train_model_v4,
    gp_des = paste(group, ", volume: ",
      dim(dat)[1], ", ptg: ",
      round(dim(dat)[1] / dim(test)[1], 3) * 100,
      "%",
      sep = ""
    ),
    num_group = num_g,
    ref_pos = ref_pos_g
  )
  print(a$table)
  print(a$plot)
}

#'
#' ### Lift Charts (Model Comparisons on Testing)
#+ echo = F, warning = F, message = F, fig.width=12, results = 'asis'
a <- lift_chart(test,
  model = train_model_v4,
  num_g = 10
)
a$tab
a$plot
acc_score <- numeric(10)
i <- 1
for (group in sort(unique(test$tenure))) {
  print(group)
  dat <- test %>%
    filter(tenure == group)
  if (dim(dat)[1] > 500) {
    num_g <- 10
  } else {
    num_g <- 5
  }
  a <- lift_chart(dat,
    model = train_model_v4,
    gp_des = paste(group, ", volume: ",
      dim(dat)[1], ", ptg: ",
      round(dim(dat)[1] / dim(test)[1], 3) * 100,
      "%",
      sep = ""
    ),
    num_group = num_g
  )
  print(a$tab)
  print(a$plot)
  acc_score[i] <- a$acc_score
  i <- i + 1
}

#'
#' The area under the lift curve overall of this model is `r a$acc_score`.
#' The area under the lift curve overall after controlling policy tenureis
#' `r sum(acc_score)`.
#'
#' Also, from the PPV plot seperate by tenure, they out-perform the
#' baseline rate almost in all tenure group. Next step is to calculate
#' the business value based on those PPVs plot, i.e., if only focusing on
#' top 20% of populuation targeted by the data, what is the value look like.
#' Meanwhile, keep working on advisor features and their derivatives and apply
#' ML tools to find reference models and futher develop new features (interaction
#' effects potentially) motivated.
#' 
#' Another immediate next step is construct CI for all A/E plots to reflect the
#' uncertainty arising from actual data.







############################################################################
# preperation
#+ echo = F, warning = F, message = F, fig.width=12, results = 'hide'








## book study
# library(gss)
# data(wesdr)
#
# k=4
# b <- gam(ret ~ rd1_c + s(dur,k=k) + s(gly,k=k) + s(bmi,k=k) +
#            ti(dur,gly,k=k) + ti(dur,bmi,k=k) + ti(gly,bmi,k=k),
#          select=F, data=wesdr, family=binomial(), method="ML",
#          gamma = 1)
# b
# summary(b)
# b <- gam(ret ~ rd1_c + s(dur,k=k) + s(gly,k=k) + s(bmi,k=k) +
#            ti(dur,gly,k=k) + ti(dur,bmi,k=k) + ti(gly,bmi,k=k),
#          select=T, data=wesdr, family=binomial(), method="ML",
#          gamma = 1)
# b
# summary(b)
#
#
#
#
# # random effect
# wesdr$rd1<-rnorm(dim(wesdr)[1])
# wesdr <- wesdr %>%
#   mutate(rd1_c = case_when(rd1 < -0.5 ~ "a",
#                            rd1 < 0 ~ "b",
#                            rd1 < 0.5 ~ "c",
#                            TRUE ~ "d"))
# wesdr$rd1_c <- as.factor(wesdr$rd1_c)
#
# b <- gam(ret ~ s(rd1_c, bs = "re")+ s(dur,k=k) + s(gly,k=k) + s(bmi,k=k) +
#            ti(dur,gly,k=k) + ti(dur,bmi,k=k) + ti(gly,bmi,k=k),
#          select=T, data=wesdr, family=binomial(), method="ML",
#          gamma = 1)
# b
# summary(b)
#
#
# require(nlme)
# b0 <- lme(travel~1,data=Rail,~1|Rail,method="REML")
#
# b <- gam(travel~s(Rail,bs="re"),data=Rail,method="REML")
# b
# summary(b)
# intervals(b0)
# gam.vcomp(b)
# anova(b)
# plot(b)
#
#
#
# #  study of gam.plot
#
# ## fake some data...
# f1 <- function(x) {exp(2 * x)}
# f2 <- function(x) {
#   0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
# }
# f3 <- function(x) {x*0}
#
# n<-200
# sig2<-4
# x0 <- rep(1:4,50)
# x1 <- runif(n, 0, 1)
# x2 <- runif(n, 0, 1)
# x3 <- runif(n, 0, 1)
# e <- rnorm(n, 0, sqrt(sig2))
# y <- 2*x0 + f1(x1) + f2(x2) + f3(x3) + e
# x0 <- factor(x0)
#
# ## fit and plot...
# b<-gam(y~x0+s(x1)+s(x2)+s(x3))
# plot(b,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
# plot(b,pages=1,seWithMean=TRUE) ## better coverage intervals
#
#
# b
# summary(b)
# par(mfrow=c(1,1))
# plot(b,pages=1)
# par(mfrow=c(2,2))
# gam.check(b,
#           type = "deviance",
#           pch=19,
#           cex=.1
# )
#
#
#
#
#
# ## just parametric term alone...
# termplot(b,terms="x0",se=TRUE)
#
# ## more use of color...
# op <- par(mfrow=c(2,2),bg="blue")
# x <- 0:1000/1000
# for (i in 1:3) {
#   plot(b,select=i,rug=FALSE,col="green",
#        col.axis="white",col.lab="white",all.terms=TRUE)
#   for (j in 1:2) axis(j,col="white",labels=FALSE)
#   box(col="white")
#   eval(parse(text=paste("fx <- f",i,"(x)",sep="")))
#   fx <- fx-mean(fx)
#   lines(x,fx,col=2) ## overlay `truth' in red
# }
# par(op)
#
# ## example with 2-d plots, and use of schemes...
# b1 <- gam(y~x0+s(x1,x2)+s(x3))
# op <- par(mfrow=c(2,2))
#
# plot.gam(b1,
#          rug = T,
#          all.terms=TRUE,
#          shade=TRUE,
#          shade.col=2,
#          residuals = T,
#          pages = 1)
#
#
# plot(b1,all.terms=TRUE)
# par(op)
# op <- par(mfrow=c(2,2))
# plot(b1,all.terms=TRUE,scheme=1)
# par(op)
# op <- par(mfrow=c(2,2))
# plot(b1,all.terms=TRUE,scheme=c(2,1))
# par(op)
#
# ## 3 and 4 D smooths can also be plotted
# dat <- gamSim(1,n=400)
# b1 <- gam(y~te(x0,x1,x2,d=c(1,2),k=c(5,15))+s(x3),data=dat)
#
# ## Now plot. Use cex.lab and cex.axis to control axis label size,
# ## n3 to control number of panels, n2 to control panel grid size,
# ## scheme=1 to get greyscale...
#
# plot(b1,pages=1)
#
#
# # beta regression example
#
# ## Simulate some beta data...
# set.seed(3);n<-400
# dat <- gamSim(1,n=n)
# dat$z <- c(rep(0,200),rep(1,200))
# mu <- binomial()$linkinv(dat$f/4-2+2*dat$z)
# phi <- .5
# a <- mu*phi;b <- phi - a;
# dat$y <- rbeta(n,a,b)
#
# bm <- gam(y~ s(f) ,
#           family=betar(link="logit"),
#           data=dat)
#
# ae_analysis <- function(model = train_model_v1,
#                         dat_train = train,
#                         ...){
#
#   dat_train$fit<-fitted(model)
#   dat_train$ae <- dat_train$y/dat_train$fit
#
#   dat_train_sum <- dat_train %>%
#     group_by(...) %>%
#     summarise(actual=mean(y),
#               expected=mean(fit),
#               ae=round(mean(ae),2))
#   dat_train_sum
# }
#
# ae_analysis(model=bm, dat_train = dat, z)
#
# bm
# summary(bm)
# plot(bm,pages=1)
# termplot(bm,terms="z",se=TRUE)
# plot.gam(bm,
#          rug = T,
#          all.terms=TRUE,
#          shade=TRUE,
#          shade.col=2,
#          residuals = F,
#          pages = 1,
#          trans = function(x)exp(x)/(1+exp(x)))
#
# plot.gam(bm,
#          rug = T,
#          all.terms=TRUE,
#          shade=TRUE,
#          shade.col=2,
#          residuals = F,
#          pages = 1,
#          trans = I)
#
# par(mfrow=c(2,2))
# gam.check(bm,
#           type = "deviance",
#           pch=19,
#           cex=.1
# )
#
#
#
#
# #plot.gam plot
#
# set.seed(0)
# ## fake some data...
# f1 <- function(x) {exp(2 * x)}
# f2 <- function(x) {
#   0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
# }
# f3 <- function(x) {x*0}
#
# n<-200
# sig2<-4
# x0 <- rep(1:4,50)
# x1 <- runif(n, 0, 1)
# x2 <- runif(n, 0, 1)
# x3 <- runif(n, 0, 1)
# e <- rnorm(n, 0, sqrt(sig2))
# y <- 2*x0 + f1(x1) + f2(x2) + f3(x3) + e
# x0 <- factor(x0)
#
# ## fit and plot...
# b<-gam(y~x0+s(x1)+s(x2)+s(x3))
# plot(b,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
# plot(b,pages=1,seWithMean=TRUE) ## better coverage intervals
#
# ## just parametric term alone...
# termplot(b,terms="x0",se=TRUE)
#
# ## more use of color...
# op <- par(mfrow=c(2,2),bg="blue")
# x <- 0:1000/1000
# for (i in 1:3) {
#   plot(b,select=i,rug=FALSE,col="green",
#        col.axis="white",col.lab="white",all.terms=TRUE)
#   for (j in 1:2) axis(j,col="white",labels=FALSE)
#   box(col="white")
#   eval(parse(text=paste("fx <- f",i,"(x)",sep="")))
#   fx <- fx-mean(fx)
#   lines(x,fx,col=2) ## overlay `truth' in red
# }
# par(op)
#
# ## example with 2-d plots, and use of schemes...
# b1 <- gam(y~x0+s(x1,x2)+s(x3))
# op <- par(mfrow=c(2,2))
# plot(b1,all.terms=TRUE)
# par(op)
# op <- par(mfrow=c(2,2))
# plot(b1,all.terms=TRUE,scheme=1)
# par(op)
# op <- par(mfrow=c(2,2))
# plot(b1,all.terms=TRUE,scheme=c(2,1))
# par(op)
#
# ## 3 and 4 D smooths can also be plotted
# dat <- gamSim(1,n=400)
# b1 <- gam(y~te(x0,x1,x2,d=c(1,2),k=c(5,15))+s(x3),data=dat)
#
# ## Now plot. Use cex.lab and cex.axis to control axis label size,
# ## n3 to control number of panels, n2 to control panel grid size,
# ## scheme=1 to get greyscale...
#
# plot(b1,pages=1)
#
#
# # term plot example
# ## requires recommended package MASS
# hills.lm <- lm(log(time) ~ log(climb)+log(dist), data = MASS::hills)
# termplot(hills.lm, partial.resid = TRUE, smooth = panel.smooth,
#          terms = "log(dist)", main = "Original")
# termplot(hills.lm, transform.x = TRUE,
#          partial.resid = TRUE, smooth = panel.smooth,
#          terms = "log(dist)", main = "Transformed",
#          se=T)
#
#
# # lift chart study
# library(rpart)
# layout(matrix(c(1,2), 2, 1))
# data(CCS)
# CCS$Sample <- create.samples(CCS, est=0.4, val=0.4)
# CCSEst <- CCS[CCS$Sample == "Estimation",]
# CCS.glm <- glm(MonthGive ~ DonPerYear + LastDonAmt + Region + YearsGive,
#                family=binomial(logit), data=CCSEst)
# library(rpart)
# CCS.rpart <- rpart(MonthGive ~ DonPerYear + LastDonAmt + Region + YearsGive,
#                    data=CCSEst, cp=0.0074)
# CCSVal <- CCS[CCS$Sample == "Validation",]
# lift.chart(c("CCS.glm", "CCS.rpart"), data=CCSVal, targLevel="Yes",
#            trueResp=0.01, type="cumulative", sub="Validation")
# lift.chart(c("CCS.glm", "CCS.rpart"), data=CCSVal, targLevel="Yes",
#            trueResp=0.01, type="incremental", sub="Validation")
