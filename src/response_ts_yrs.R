#' ---
#' title: Prop Capture, response of GIA
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
#' ## Introduction
#'
#' In this section, I will focus on analyzing GIA % change over time first. Then I will look at other MM fund.
#'
#' load data of 09-2019 to 04-2020
#'
#+ echo = F, warning = F, results = 'hide'

# Pull one year data: 8 files
cft_yrs_names <- paste("./data/",
  list.files(
    path = "./data/",
    pattern = "^cfr_[rt]"
  ),
  sep = ""
)
cft_yrs_names <- c(cft_yrs_names, "./data/cfr_12_31_2019.csv")

var_int <- c(
  "date",
  "Plan.ID",
  "Contract",
  "ContractNo",
  "PlatformName",
  "PlatformID",
  "DCorDB",
  "ICU.Cd",
  "GIA",
  "Total",
  "TotalContractAssets",
  "GIA..",
  "GIA.pct",
  "Total.Proprietary.Assets.."
)


cft_yrs_files <- lapply(cft_yrs_names, function(i) {
  cat("\n", i, "\n")
  date_part <- gsub("_", "/", str_extract(i, "[:digit:]{1,2}\\_[:digit:]{1,2}\\_[:digit:]{1,4}"))
  cat("\n", date_part, "\n")

  if (!is.na(str_extract(i, "reflex"))) {
    dat <- data.frame(cbind(
      date = date_part,
      PlatformName = "ReFlex",
      read.csv(i,
        na.strings = c("NA", "-", "", ".", NULL, " "),
        header = TRUE,
        colClasses = c("character"),
        check.names = F
      )
    ))


    dat <- dat %>%
      select(one_of(var_int)) %>%
      mutate(
        date = as.character(date),
        gia_p = as.numeric(gsub("%", "", GIA.pct)),
        prop_p = as.numeric(gsub("%", "", Total.Proprietary.Assets..)),
        ContractNo = Contract,
        PlatformName = as.character(PlatformName)
      ) %>%
      select("date", "ContractNo", "PlatformName", gia_p, prop_p)
  } else if (!is.na(str_extract(i, "trac"))) {
    dat <- data.frame(cbind(
      date = date_part,
      read.csv(i,
        na.strings = c("NA", "-", "", ".", NULL, " "),
        header = TRUE,
        colClasses = c("character"),
        check.names = F
      )
    ))

    #  cat(names(dat))


    dat <- dat %>%
      select(one_of(var_int)) %>%
      mutate(
        date = as.character(date),
        ContractNo = Plan.ID,
        PlatformName = case_when(
          str_detect(ICU.Cd, "TRAC") ~ "TRAC",
          TRUE ~ ICU.Cd
        ),
        gia_p = as.numeric(GIA) / as.numeric(gsub(",", "", Total)) * 100,
        prop_p = as.numeric(gsub("%", "", Total.Proprietary.Assets..)),
        gia = as.numeric(GIA),
        total = as.numeric(gsub(",", "", Total))
      ) %>%
      select("date", "ContractNo", "PlatformName", gia_p, prop_p)
  } else {
    dat <- data.frame(cbind(
      date = date_part,
      read.csv(i,
        na.strings = c("NA", "-", "", ".", NULL, " "),
        header = TRUE,
        colClasses = c("character"),
        check.names = F
      )
    ))
    # baseline dataset setting
    dat <- dat %>%
      filter(DCorDB == "DC", PlatformID %in% c("1", "2", "3")) %>%
      mutate(
        gia_p = as.numeric(GIA) / as.numeric(TotalContractAssets) * 100,
        prop_p = as.numeric(Total.Proprietary.Assets..) * 100,
        date = as.character(date),
        gia = as.numeric(GIA),
        total = TotalContractAssets
      ) %>%
      select("date", "ContractNo", "PlatformName", gia_p, prop_p)
  }
  cat("\n", dim(dat), "\n")
  return(dat)
})



## look at column names match
col_names_cft_yrs <- lapply(cft_yrs_files, function(i) {
  names(i)
})
identicalValue <- function(x, y) if (identical(x, y)) x else FALSE
Reduce(identicalValue, col_names_cft_yrs) # if print the column names, then it indicates they are all have the same col. names

## stack data together
mydata_yrs <- do.call("rbind", cft_yrs_files)
## name format
names(mydata_yrs) <- tolower(gsub(x = names(mydata_yrs), pattern = "\\.", replacement = "_"))

# take a look at where NA come from
mydata_yrs %>%
  group_by(platformname, date) %>%
  summarise(
    total = n(),
    total_prop_na = sum(is.na(prop_p)),
    total_gia_na = sum(is.na(gia_p))
  ) %>%
  arrange(platformname, as.Date(as.character(date), format = "%m/%d/%Y"))
# mydata_yrs <- mydata_yrs[,1:4]

### align dates
dim(mydata_yrs)
mydata_yrs <- mydata_yrs %>%
  mutate(date = case_when(
    date == "09/30/2015" ~ "12/31/2015",
    TRUE ~ date
  ))
dim(mydata_yrs)

# seperate prop data and gia data
mydata_prop_yrs <- mydata_yrs[, c(1:3, 5)]
dim(mydata_yrs)
dim(mydata_prop_yrs)
# Only focus on the full data, no NAs.
#

dim(mydata_yrs)
mydata_yrs <- mydata_yrs[, 1:4] %>%
  na.omit()
dim(mydata_yrs)

dim(mydata_prop_yrs)
mydata_prop_yrs <- mydata_prop_yrs %>%
  na.omit()
dim(mydata_prop_yrs)

# 140,740 out of 148,675 records with no missing gia_p are left.
#


mydata_yrs_qc1 <- mydata_yrs %>%
  ungroup() %>%
  group_by(date, contractno, platformname) %>%
  summarise(total = n())
table(mydata_yrs_qc1$total)
mydata_yrs_qc1_ls <- mydata_yrs_qc1 %>%
  filter(total > 1)

mydata_prop_yrs_qc1 <- mydata_prop_yrs %>%
  ungroup() %>%
  group_by(date, contractno, platformname) %>%
  summarise(total = n())
table(mydata_prop_yrs_qc1$total)



# There is a duplicate policy, I will only keep one record
dim(mydata_yrs)
dim(mydata_prop_yrs)
mydata_yrs <- mydata_yrs %>%
  distinct_all()
mydata_prop_yrs <- mydata_prop_yrs %>%
  distinct_all()
dim(mydata_yrs)
dim(mydata_prop_yrs)


mydata_yrs_qc <- mydata_yrs %>%
  ungroup() %>%
  group_by(date, contractno, platformname) %>%
  summarise(total = n())
table(mydata_yrs_qc$total)

mydata_prop_yrs_qc <- mydata_prop_yrs %>%
  ungroup() %>%
  group_by(date, contractno, platformname) %>%
  summarise(total = n())
table(mydata_prop_yrs_qc$total)


#' For GIA, only look at policies that appears in all 5 years
#+ echo = F 

mydata_yrs_qc <- mydata_yrs %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
  summarise(total_yrs = n())
table(mydata_yrs_qc$platformname, mydata_yrs_qc$total_yrs)

#' For Prop Fund, it depends on the availablity of platform data
#+ echo = F 

mydata_yrs_prop_qc <- mydata_prop_yrs %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
  summarise(total_yrs = n())
table(mydata_yrs_prop_qc$platformname, mydata_yrs_prop_qc$total_yrs)



#' Next, I will look at the time series split by platform. For prop fund,
#' I will look at policies that appear in all five years from ReFlex platform,
#' which occupy around 41.6% of data, and policies that appear in three years from
#' other platform.
#'
#' For GIA, I will look at policies that appears in all five years, which occupy around
#' 41% % in total TRAC, 26.1\% in total OMNI, and , 41.6\% seperately.
#'
#' From sampling perspective, a sample size of more than 100 is acceptable, thus
#' spliting out data into 10 quantile groups is fine.
#+ echo = F, results = 'hide'

dim(mydata_yrs)
mydata_yrs <- mydata_yrs %>%
  ungroup() %>%
  left_join(mydata_yrs_qc, by = c("contractno", "platformname"))

mydata_prop_yrs <- mydata_prop_yrs %>%
  ungroup() %>%
  left_join(mydata_yrs_prop_qc, by = c("contractno", "platformname"))
dim(mydata_yrs)

# qc
table(mydata_yrs$platformname, mydata_yrs$total_yrs)
table(mydata_prop_yrs$platformname, mydata_prop_yrs$total_yrs)

#' Look at distributions of Prop Fund percentage
#+ echo = F 
summary(mydata_yrs$prop_p)
hist(mydata_prop_yrs$prop_p, xlab = "Prop Fund %", main = "Prop Fund Spreads")

#' Look at distributions of GIA percentage
#+ echo = F 
summary(mydata_yrs$gia_p)
hist(mydata_yrs$gia_p, xlab = "GIA %", main = "GIA Spreads")

#' treat all Prop/GIA percentage bigger than 100 as 100,
#+ echo = F, results = 'hide'
dim(mydata_yrs)
dim(mydata_prop_yrs)
mydata_yrs <- mydata_yrs %>%
  mutate(gia_p = ifelse(gia_p > 100, 100, gia_p))
mydata_prop_yrs <- mydata_prop_yrs %>%
  mutate(prop_p = ifelse(prop_p > 100, 100, prop_p))
dim(mydata_yrs)
dim(mydata_prop_yrs)


#' double check the Prop Fund distribution
#+ echo = F
summary(mydata_prop_yrs$prop_p)
hist(mydata_prop_yrs$prop_p, xlab = "Prop Fund %", main = "Prop Fund Spreads")

#' double check the GIA distribution
#+ echo = F
summary(mydata_yrs$gia_p)
hist(mydata_yrs$gia_p, xlab = "GIA %", main = "GIA Spreads")

#' ## Prop Fund Performance on Contract Level
#'
#' ### TRAC Platform
#'

#+ echo = F, warning =F, results = 'hide'
mydata_prop_yrs_trac <- mydata_prop_yrs %>%
  ungroup() %>%
  filter(platformname == "TRAC", total_yrs == 3)


#'
#' * For each contract, compute its prop% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group prop_p means vary
#' over years (3 years)
#'
#+ echo = F, warning =F, results = 'hide'

dim(mydata_prop_yrs_trac)
seq_f <- quantile(mydata_prop_yrs_trac$prop_p[which(mydata_prop_yrs_trac$prop_p != 0 & mydata_prop_yrs_trac$prop_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_prop_yrs_trac <- mydata_prop_yrs_trac %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(prop_p_gp = cut(prop_p,
    breaks = unique(c(-0.1, round(seq_f, 1), 100)),
    right = T
  )) %>%
  select(contractno, platformname, prop_p_gp) %>%
  right_join(mydata_prop_yrs_trac, by = c("contractno", "platformname"))
dim(mydata_prop_yrs_trac)



#+ echo = F
tab <- mydata_prop_yrs_trac %>%
  na.omit() %>%
  group_by(date, prop_p_gp) %>%
  summarize(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_p_md = median(prop_p),
    prop_p_min = min(prop_p),
    prop_p_max = max(prop_p)
  )

#' Groups created based on 12/31/2019 data
#+ echo = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, prop_p_gp, total, prop_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")



# smaller group
tab_s <- mydata_prop_yrs_trac %>%
  filter(prop_p_gp %in% c(
    "(-0.1,0]",
    "(0,1.3]",
    "(1.3,4]"
  )) %>%
  group_by(date, prop_p_gp) %>%
  summarize(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_p_md = median(prop_p),
    prop_p_min = min(prop_p),
    prop_p_max = max(prop_p)
  )
tab_s$date <- as.Date(as.character(tab_s$date), format = "%m/%d/%Y")
#'
#' Track prop Group mean change in each of 3 years
#'
#' #### prop Group Means
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_avg, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))




#' smaller group
#+ fig.width=12, echo = F
g <- ggplot(tab_s, aes(y = prop_p_avg, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab_s %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))



#' #### prop Group Medians
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_md, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))



#' smaller group
#+ fig.width=12, echo = F
g <- ggplot(tab_s, aes(y = prop_p_md, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab_s %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))



#' #### prop Groups Maxes
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_max, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Max %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' #### prop Group Mins
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_min, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Min %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#'
#' #### prop Groups and prop Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the prop
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#'
#+ echo = F
tab <- mydata_prop_yrs_trac %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
  summarise(
    total = n(),
    prop_p_min = min(prop_p),
    prop_p_md = median(prop_p),
    prop_p_avg = mean(prop_p),
    prop_p_max = max(prop_p),
    prop_p_gap = round(prop_p_max - prop_p_min, 2)
  )




seq_gap <- quantile(tab$prop_p_gap[which(tab$prop_p_gap != 0 & tab$prop_p_gap != 100)], probs = seq(0, 1, 0.1))

tab_t <- tab %>%
  ungroup() %>%
  mutate(prop_gap_gp = cut(prop_p_gap,
    breaks = c(
      -0.1,
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, platformname, prop_gap_gp, prop_p_gap)

#' see what distribution looks like in prop gap


#' Based on count
#+ warning = F, echo = F
table(tab_t$prop_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#' Based on proportion
#+ echo = F
prop.table(table(tab_t$prop_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


# add prop gap distribution of each contract back to original data
#+ echo = F, results = 'hide'
dim(mydata_prop_yrs_trac)
mydata_prop_yrs_trac <- mydata_prop_yrs_trac %>%
  left_join(tab_t, by = c("contractno", "platformname"))
dim(mydata_prop_yrs_trac)

tab <- mydata_prop_yrs_trac %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  summarise(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_gap_avg = round(mean(as.numeric(prop_p_gap)), 2),
    prop_gap_median = round(median(as.numeric(prop_p_gap)), 2),
  )
#' Look at how prop distributed in each of prop group
#+ warning = F, echo = F


#'
#'
#+ echo = F
ggplot(tab, aes(y = total, x = prop_p_gp, fill = prop_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline prop Group of 12/31/2019") +
  ylab("Number of Contracts in prop Group X Gap") +
  labs(fill = "prop Gaps in 8 Months") +
  ggtitle("prop Gap Distribution by prop Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = prop_p_gp, fill = prop_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline prop Group of 12/31/2019") +
  ylab("Distribution of the Number of Contracts in prop Group X Gap") +
  labs(fill = "prop Gaps in 8 Months") +
  ggtitle("prop Gap Distribution by prop Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )




# QC the summarization in above step
#
# select a representative in each of the group based on prop_p_gp and prop_gap_gp
# on the 12-31-2019
#

qc_prop <- mydata_prop_yrs_trac %>%
  ungroup() %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  slice(n()) %>%
  select(contractno, platformname, prop_p_gp, prop_gap_gp)

qc_prop_data <- mydata_prop_yrs_trac %>%
  filter(
    contractno %in% qc_prop$contractno,
    platformname %in% qc_prop$platformname
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, platformname, prop_p, prop_p_gp, prop_p_gap, prop_gap_gp) %>%
  arrange(contractno, platformname, as.Date(date))

# After looking some cases, the features of prop percent groups are correct.



#'
#' ### OMNI Platform
#'
#+ echo = F, results = 'hide'


mydata_prop_yrs_omni <- mydata_prop_yrs %>%
  ungroup() %>%
  filter(platformname == "OMNI", total_yrs == 3)

#'
#' * For each contract, compute its prop% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group prop_p means vary
#' over years (3 years)
#'
#+ echo = F, results = 'hide'


dim(mydata_prop_yrs_omni)
seq_f <- quantile(mydata_prop_yrs_omni$prop_p[which(mydata_prop_yrs_omni$prop_p != 0 & mydata_prop_yrs_omni$prop_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_prop_yrs_omni <- mydata_prop_yrs_omni %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(prop_p_gp = cut(prop_p,
    breaks = unique(c(-0.1, round(seq_f, 1), 100)),
    right = T
  )) %>%
  select(contractno, platformname, prop_p_gp) %>%
  right_join(mydata_prop_yrs_omni, by = c("contractno", "platformname"))
dim(mydata_prop_yrs_omni)

tab <- mydata_prop_yrs_omni %>%
  na.omit() %>%
  group_by(date, prop_p_gp) %>%
  summarize(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_p_md = median(prop_p),
    prop_p_min = min(prop_p),
    prop_p_max = max(prop_p)
  )
#' Groups created based on 12/31/2019 data
#+ warning = F, echo = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, prop_p_gp, total, prop_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")

#' Track prop Group mean change in each of 3 years
#'
#' #### prop Group Means
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_avg, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' #### prop Group Medians
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_md, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))



#' #### prop Groups Maxes
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_max, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Max %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' #### prop Group Mins
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_min, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Min %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#'
#' #### prop Groups and prop Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the prop
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#+ echo = F, results = 'hide'

tab <- mydata_prop_yrs_omni %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
  summarise(
    total = n(),
    prop_p_min = min(prop_p),
    prop_p_md = median(prop_p),
    prop_p_avg = mean(prop_p),
    prop_p_max = max(prop_p),
    prop_p_gap = round(prop_p_max - prop_p_min, 2)
  )
seq_gap <- quantile(tab$prop_p_gap[which(tab$prop_p_gap != 0 & tab$prop_p_gap != 100)], probs = seq(0, 1, 0.1))
tab_t <- tab %>%
  ungroup() %>%
  mutate(prop_gap_gp = cut(prop_p_gap,
    breaks = c(
      -0.1,
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, platformname, prop_gap_gp, prop_p_gap)

#' see what distribution looks like in prop gap
#'
#' Based on count
#+ warning = F, echo = F
table(tab_t$prop_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#' Based on frequency
#+ warning = F, echo = F

prop.table(table(tab_t$prop_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#+ echo = F, results= 'hide'
# add prop gap distribution of each contract back to original data
dim(mydata_prop_yrs_omni)
mydata_prop_yrs_omni <- mydata_prop_yrs_omni %>%
  left_join(tab_t, by = c("contractno", "platformname"))
dim(mydata_prop_yrs_omni)

tab <- mydata_prop_yrs_omni %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  summarise(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_gap_avg = round(mean(as.numeric(prop_p_gap)), 2),
    prop_gap_median = round(median(as.numeric(prop_p_gap)), 2),
  )

#'
#' Look at how prop distributed in each of prop group
#+ warning = F, echo = F



ggplot(tab, aes(y = total, x = prop_p_gp, fill = prop_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline prop Group of 12/31/2019") +
  ylab("Number of Contracts in prop Group X Gap") +
  labs(fill = "prop Gaps in 8 Months") +
  ggtitle("prop Gap Distribution by prop Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = prop_p_gp, fill = prop_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline prop Group of 12/31/2019") +
  ylab("Distribution of the Number of Contracts in prop Group X Gap") +
  labs(fill = "prop Gaps in 8 Months") +
  ggtitle("prop Gap Distribution by prop Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )
# QC the summarization in above step

# select a representative in each of the group based on prop_p_gp and prop_gap_gp
# on the 12-31-2019
#+ echo = F, results = 'hide' 

qc_prop <- mydata_prop_yrs_omni %>%
  ungroup() %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  slice(n()) %>%
  select(contractno, platformname, prop_p_gp, prop_gap_gp)

qc_prop_data <- mydata_prop_yrs_omni %>%
  filter(
    contractno %in% qc_prop$contractno,
    platformname %in% qc_prop$platformname
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, platformname, prop_p, prop_p_gp, prop_p_gap, prop_gap_gp) %>%
  arrange(contractno, platformname, as.Date(date))

# After looking some cases, the features of prop percent groups are correct.
#'
#' ### ReFlex Platform
#'
#+ echo = F, results = 'hide'
mydata_prop_yrs_reflex <- mydata_prop_yrs %>%
  ungroup() %>%
  filter(platformname == "ReFlex", total_yrs == 5)
#'
#' * For each contract, compute its prop% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group prop_p means vary
#' over years (5 years)
#'
#+ echo = F, results = 'hide'

dim(mydata_prop_yrs_reflex)
seq_f <- quantile(mydata_prop_yrs_reflex$prop_p[which(mydata_prop_yrs_reflex$prop_p != 0 & mydata_prop_yrs_reflex$prop_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_prop_yrs_reflex <- mydata_prop_yrs_reflex %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(prop_p_gp = cut(prop_p,
    breaks = unique(c(-0.1, round(seq_f, 1), 100)),
    right = T
  )) %>%
  select(contractno, platformname, prop_p_gp) %>%
  right_join(mydata_prop_yrs_reflex, by = c("contractno", "platformname"))
dim(mydata_prop_yrs_reflex)


#' Track prop Group mean change in each of 5 years
#+ echo = F, results = 'hide'
tab <- mydata_prop_yrs_reflex %>%
  na.omit() %>%
  group_by(date, prop_p_gp) %>%
  summarize(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_p_md = median(prop_p),
    prop_p_min = min(prop_p),
    prop_p_max = max(prop_p)
  )
#' Groups created based on 12/31/2019 data
#+ warning = F, echo = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, prop_p_gp, total, prop_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")


#' #### prop Group Means
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_avg, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Mean %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))






#' #### prop Group Medians
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_md, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Median %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))



#' #### prop Groups Maxes
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_max, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Max %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#' #### prop Group Mins
#'
#+ fig.width=12, echo = F
g <- ggplot(tab, aes(y = prop_p_min, x = date, color = prop_p_gp)) +
  geom_point(size = 1) +
  geom_line(size = 0.4) +
  scale_x_date(date_labels = "%m-%d-%Y", name = "date", breaks = tab$date) +
  scale_y_continuous(
    name = "prop Min %",
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  labs(color = "prop Group") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(angle = 90)
  )

g + ggplot(tab %>% filter(date == "2019-12-31"), aes(y = total, x = prop_p_gp)) +
  geom_bar(mapping = aes(x = prop_p_gp, y = total), stat = "identity", fill = "tan1", colour = "sienna3") +
  xlab("prop Group") +
  labs(caption = "prop Group on 12-31-2019 Data") +
  theme(axis.text.x = element_text(angle = 90))

#'
#' #### prop Groups and prop Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the prop
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#+ echo = F, results = 'hide'

tab <- mydata_prop_yrs_reflex %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
  summarise(
    total = n(),
    prop_p_min = min(prop_p),
    prop_p_md = median(prop_p),
    prop_p_avg = mean(prop_p),
    prop_p_max = max(prop_p),
    prop_p_gap = round(prop_p_max - prop_p_min, 2)
  )




seq_gap <- quantile(tab$prop_p_gap[which(tab$prop_p_gap != 0 & tab$prop_p_gap != 100)], probs = seq(0, 1, 0.1))

tab_t <- tab %>%
  ungroup() %>%
  mutate(prop_gap_gp = cut(prop_p_gap,
    breaks = c(
      -0.1,
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, platformname, prop_gap_gp, prop_p_gap)

#' see what distribution looks like in prop gap
#'
#' Based on count
#+ warning = F, echo = F
table(tab_t$prop_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#' Based on frequency
#+ warning = F, echo = F, results= 'hide'

prop.table(table(tab_t$prop_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


# add prop gap distribution of each contract back to original data
dim(mydata_prop_yrs_reflex)
mydata_prop_yrs_reflex <- mydata_prop_yrs_reflex %>%
  left_join(tab_t, by = c("contractno", "platformname"))
dim(mydata_prop_yrs_reflex)

tab <- mydata_prop_yrs_reflex %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  summarise(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_gap_avg = round(mean(as.numeric(prop_p_gap)), 2),
    prop_gap_median = round(median(as.numeric(prop_p_gap)), 2),
  )
#'
#' Look at how prop distributed in each of prop group
#+ warning = F, echo = F

ggplot(tab, aes(y = total, x = prop_p_gp, fill = prop_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline prop Group of 12/31/2019") +
  ylab("Number of Contracts in prop Group X Gap") +
  labs(fill = "prop Gaps in 8 Months") +
  ggtitle("prop Gap Distribution by prop Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = prop_p_gp, fill = prop_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline prop Group of 12/31/2019") +
  ylab("Distribution of the Number of Contracts in prop Group X Gap") +
  labs(fill = "prop Gaps in 8 Months") +
  ggtitle("prop Gap Distribution by prop Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )



# QC the summarization in above step
#
# select a representative in each of the group based on prop_p_gp and prop_gap_gp
# on the 12-31-2019
#

qc_prop <- mydata_prop_yrs_reflex %>%
  ungroup() %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  slice(n()) %>%
  select(contractno, platformname, prop_p_gp, prop_gap_gp)

qc_prop_data <- mydata_prop_yrs_reflex %>%
  filter(
    contractno %in% qc_prop$contractno,
    platformname %in% qc_prop$platformname
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, platformname, prop_p, prop_p_gp, prop_p_gap, prop_gap_gp) %>%
  arrange(contractno, platformname, as.Date(date))

# After looking some cases, the features of prop percent groups are correct.




#' ## GIA Performance on Contract Level
#'
#' ### TRAC Platform
#'


#+ echo = F, warning =F, results = 'hide'
mydata_yrs_trac <- mydata_yrs %>%
  ungroup() %>%
  filter(platformname == "TRAC", total_yrs == 5)


#'
#' * For each contract, compute its GIA% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group gia_p means vary
#' over years (5 years)
#'
#+ echo = F, warning =F, results = 'hide'

dim(mydata_yrs_trac)
seq_f <- quantile(mydata_yrs_trac$gia_p[which(mydata_yrs_trac$gia_p != 0 & mydata_yrs_trac$gia_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_yrs_trac <- mydata_yrs_trac %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(gia_p_gp = cut(gia_p,
    breaks = unique(c(-0.1, round(seq_f, 1), 100)),
    right = T
  )) %>%
  select(contractno, platformname, gia_p_gp) %>%
  right_join(mydata_yrs_trac, by = c("contractno", "platformname"))
dim(mydata_yrs_trac)



#+ echo = F
tab <- mydata_yrs_trac %>%
  na.omit() %>%
  group_by(date, gia_p_gp) %>%
  summarize(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_p_md = median(gia_p),
    gia_p_min = min(gia_p),
    gia_p_max = max(gia_p)
  )

#' Groups created based on 12/31/2019 data
#+ echo = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, gia_p_gp, total, gia_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")



# smaller group
tab_s <- mydata_yrs_trac %>%
  filter(gia_p_gp %in% c(
    "(-0.1,0]",
    "(0,0.3]",
    "(0.3,1.2]"
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


#'
#' Track GIA Group mean change in each of 5 years
#'
#' #### GIA Group Means
#'
#+ fig.width=12, echo = F
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




#' smaller group
#+ fig.width=12, echo = F
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



#' #### GIA Group Medians
#'
#+ fig.width=12, echo = F
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



#' smaller group
#+ fig.width=12, echo = F
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



#' #### GIA Groups Maxes
#'
#+ fig.width=12, echo = F
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

#' #### GIA Group Mins
#'
#+ fig.width=12, echo = F
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
#' #### GIA Groups and GIA Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the GIA
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#'
#+ echo = F
tab <- mydata_yrs_trac %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
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
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, platformname, gia_gap_gp, gia_p_gap)

#' see what distribution looks like in gia gap


#' Based on count
#+ warning = F, echo = F
table(tab_t$gia_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#' Based on proportion
#+ echo = F
prop.table(table(tab_t$gia_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


# add gia gap distribution of each contract back to original data
#+ echo = F, results = 'hide'
dim(mydata_yrs_trac)
mydata_yrs_trac <- mydata_yrs_trac %>%
  left_join(tab_t, by = c("contractno", "platformname"))
dim(mydata_yrs_trac)

tab <- mydata_yrs_trac %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )
#' Look at how GIA distributed in each of GIA group
#+ warning = F, echo = F


#'
#'
#+ echo = F
ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Number of Contracts in GIA Group X Gap") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Gap Distribution by GIA Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Distribution of the Number of Contracts in GIA Group X Gap") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Gap Distribution by GIA Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )




# QC the summarization in above step
#
# select a representative in each of the group based on GIA_p_gp and GIA_gap_gp
# on the 12-31-2019
#

qc_gia <- mydata_yrs_trac %>%
  ungroup() %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  slice(n()) %>%
  select(contractno, platformname, gia_p_gp, gia_gap_gp)

qc_gia_data <- mydata_yrs_trac %>%
  filter(
    contractno %in% qc_gia$contractno,
    platformname %in% qc_gia$platformname
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, platformname, gia_p, gia_p_gp, gia_p_gap, gia_gap_gp) %>%
  arrange(contractno, platformname, as.Date(date))

# After looking some cases, the features of GIA percent groups are correct.



#'
#' ### OMNI Platform
#'
#+ echo = F, results = 'hide'


mydata_yrs_omni <- mydata_yrs %>%
  ungroup() %>%
  filter(platformname == "OMNI", total_yrs == 5)




#'
#' * For each contract, compute its GIA% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group gia_p means vary
#' over years (5 years)
#'
#+ echo = F, results = 'hide'


dim(mydata_yrs_omni)
seq_f <- quantile(mydata_yrs_omni$gia_p[which(mydata_yrs_omni$gia_p != 0 & mydata_yrs_omni$gia_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_yrs_omni <- mydata_yrs_omni %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(gia_p_gp = cut(gia_p,
    breaks = unique(c(-0.1, round(seq_f, 1), 100)),
    right = T
  )) %>%
  select(contractno, platformname, gia_p_gp) %>%
  right_join(mydata_yrs_omni, by = c("contractno", "platformname"))
dim(mydata_yrs_omni)



tab <- mydata_yrs_omni %>%
  na.omit() %>%
  group_by(date, gia_p_gp) %>%
  summarize(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_p_md = median(gia_p),
    gia_p_min = min(gia_p),
    gia_p_max = max(gia_p)
  )
#' Groups created based on 12/31/2019 data
#+ warning = F, echo = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, gia_p_gp, total, gia_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")

#' Track GIA Group mean change in each of 5 years
#'
#' #### GIA Group Means
#'
#+ fig.width=12, echo = F
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






#' #### GIA Group Medians
#'
#+ fig.width=12, echo = F
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



#' #### GIA Groups Maxes
#'
#+ fig.width=12, echo = F
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

#' #### GIA Group Mins
#'
#+ fig.width=12, echo = F
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
#' #### GIA Groups and GIA Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the GIA
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#+ echo = F, results = 'hide'

tab <- mydata_yrs_omni %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
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
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, platformname, gia_gap_gp, gia_p_gap)

#' see what distribution looks like in gia gap
#'
#' Based on count
#+ warning = F, echo = F
table(tab_t$gia_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#' Based on frequency
#+ warning = F, echo = F

prop.table(table(tab_t$gia_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#+ echo = F, results= 'hide'
# add gia gap distribution of each contract back to original data
dim(mydata_yrs_omni)
mydata_yrs_omni <- mydata_yrs_omni %>%
  left_join(tab_t, by = c("contractno", "platformname"))
dim(mydata_yrs_omni)

tab <- mydata_yrs_omni %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )

#'
#' Look at how GIA distributed in each of GIA group
#+ warning = F, echo = F



ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Number of Contracts in GIA Group X Gap") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Gap Distribution by GIA Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Distribution of the Number of Contracts in GIA Group X Gap") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Gap Distribution by GIA Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )





# QC the summarization in above step

# select a representative in each of the group based on GIA_p_gp and GIA_gap_gp
# on the 12-31-2019
#+ echo = F, results = 'hide' 

qc_gia <- mydata_yrs_omni %>%
  ungroup() %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  slice(n()) %>%
  select(contractno, platformname, gia_p_gp, gia_gap_gp)

qc_gia_data <- mydata_yrs_omni %>%
  filter(
    contractno %in% qc_gia$contractno,
    platformname %in% qc_gia$platformname
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, platformname, gia_p, gia_p_gp, gia_p_gap, gia_gap_gp) %>%
  arrange(contractno, platformname, as.Date(date))

# After looking some cases, the features of GIA percent groups are correct.




#'
#' ### ReFlex Platform
#'
#+ echo = F, results = 'hide'
mydata_yrs_reflex <- mydata_yrs %>%
  ungroup() %>%
  filter(platformname == "ReFlex", total_yrs == 5)




#'
#' * For each contract, compute its GIA% group (10 groups) at 12/31/2019,
#' , for it is the data we have to build our model potentially. Treating
#' the group information as the baseline, then look at how group gia_p means vary
#' over years (5 years)
#'
#+ echo = F, results = 'hide'

dim(mydata_yrs_reflex)
seq_f <- quantile(mydata_yrs_reflex$gia_p[which(mydata_yrs_reflex$gia_p != 0 & mydata_yrs_reflex$gia_p != 100)],
  prob = seq(0, 1, 0.1)
)
mydata_yrs_reflex <- mydata_yrs_reflex %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  mutate(gia_p_gp = cut(gia_p,
    breaks = unique(c(-0.1, round(seq_f, 1), 100)),
    right = T
  )) %>%
  select(contractno, platformname, gia_p_gp) %>%
  right_join(mydata_yrs_reflex, by = c("contractno", "platformname"))
dim(mydata_yrs_reflex)


#' Track GIA Group mean change in each of 5 years
#+ echo = F, results = 'hide'
tab <- mydata_yrs_reflex %>%
  na.omit() %>%
  group_by(date, gia_p_gp) %>%
  summarize(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_p_md = median(gia_p),
    gia_p_min = min(gia_p),
    gia_p_max = max(gia_p)
  )
#' Groups created based on 12/31/2019 data
#+ warning = F, echo = F
tab %>%
  filter(date == "12/31/2019") %>%
  select(date, gia_p_gp, total, gia_p_avg) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

tab$date <- as.Date(as.character(tab$date), format = "%m/%d/%Y")


#' #### GIA Group Means
#'
#+ fig.width=12, echo = F
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






#' #### GIA Group Medians
#'
#+ fig.width=12, echo = F
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



#' #### GIA Groups Maxes
#'
#+ fig.width=12, echo = F
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

#' #### GIA Group Mins
#'
#+ fig.width=12, echo = F
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
#' #### GIA Groups and GIA Gap Groups
#'
#' As seen in previous section, some policies could change into different groups along the time.
#' In this section, I will explore how contract polices change along the time in each of the GIA
#' Groups based on the 12/31/2019 baseline data.
#'
#' * For each contract, compute its group gap along the time, and summarize them
#'
#+ echo = F, results = 'hide'

tab <- mydata_yrs_reflex %>%
  ungroup() %>%
  group_by(contractno, platformname) %>%
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
      seq_gap,
      100
    ), right = T
  )) %>%
  select(contractno, platformname, gia_gap_gp, gia_p_gap)

#' see what distribution looks like in gia gap
#'
#' Based on count
#+ warning = F, echo = F
table(tab_t$gia_gap_gp) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
#' Based on frequency
#+ warning = F, echo = F, results= 'hide'

prop.table(table(tab_t$gia_gap_gp)) %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


# add gia gap distribution of each contract back to original data
dim(mydata_yrs_reflex)
mydata_yrs_reflex <- mydata_yrs_reflex %>%
  left_join(tab_t, by = c("contractno", "platformname"))
dim(mydata_yrs_reflex)

tab <- mydata_yrs_reflex %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )
#'
#' Look at how GIA distributed in each of GIA group
#+ warning = F, echo = F

ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Number of Contracts in GIA Group X Gap") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Gap Distribution by GIA Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(tab, aes(y = total, x = gia_p_gp, fill = gia_gap_gp)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("Baseline GIA Group of 12/31/2019") +
  ylab("Distribution of the Number of Contracts in GIA Group X Gap") +
  labs(fill = "GIA Gaps in 8 Months") +
  ggtitle("GIA Gap Distribution by GIA Group") +
  theme(
    axis.text.x = element_text(angle = 90)
  )



# QC the summarization in above step
#
# select a representative in each of the group based on GIA_p_gp and GIA_gap_gp
# on the 12-31-2019
#

qc_gia <- mydata_yrs_reflex %>%
  ungroup() %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  slice(n()) %>%
  select(contractno, platformname, gia_p_gp, gia_gap_gp)

qc_gia_data <- mydata_yrs_reflex %>%
  filter(
    contractno %in% qc_gia$contractno,
    platformname %in% qc_gia$platformname
  ) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y")) %>%
  select(date, contractno, platformname, gia_p, gia_p_gp, gia_p_gap, gia_gap_gp) %>%
  arrange(contractno, platformname, as.Date(date))

# After looking some cases, the features of GIA percent groups are correct.


#'
#'
#' ## Conclusions
#'
#' As a conclusion, both Prop and GIA performs stably along years. For the prop/GIA gap by prop/GIA group
#' distribution study, from both data and business sense from Chris, OMNI platform
#' has the biggest values in terms of prop/GIA group cutoffs and prop/GIA gap cutoffs, as it
#' contains relatively large asset size policy mainly from government segment; TRAC, as
#' an opposite, has the smallest values for it contains relatively small asset size
#' mainly from coporate plan; Reflex is in the middle, although for prop it is very closed to its
#' OMNI counterpart group. Also, for Reflex platform, the only platform that have 5 years of prop
#' fund rates data, it generate less 0s compared to GIA counterpart rates, which is not unreasonable
#' because GIA is a subset of prop fund.
#'
#'
#' Next, we could move forward
#' to the next step of feature engineering and modeling
#'
#'
#' ## Appendix - Tables of prop Gap v.s. prop Group by Platforms
#'
#' How prop Gap distributed in each of prop groups in each of those three
#' platforms, TRAC, OMNI and Reflex.
#'
#' prop Groups are based on data on 12/31/2019
#'
#' ### TRAC
#'
#+ echo = F, warning = F

tab <- mydata_prop_yrs_trac %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  summarise(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_gap_avg = round(mean(as.numeric(prop_p_gap)), 2),
    prop_gap_median = round(median(as.numeric(prop_p_gap)), 2),
  )


tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#'
#' ### OMNI
#'
#+ warning = F, echo = F 


tab <- mydata_prop_yrs_omni %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  summarise(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_gap_avg = round(mean(as.numeric(prop_p_gap)), 2),
    prop_gap_median = round(median(as.numeric(prop_p_gap)), 2),
  )



tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#'
#' ### Reflex
#'
#+ warning = F, echo = F


tab <- mydata_prop_yrs_reflex %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(prop_p_gp, prop_gap_gp) %>%
  summarise(
    total = n(),
    prop_p_avg = round(mean(prop_p), 2),
    prop_gap_avg = round(mean(as.numeric(prop_p_gap)), 2),
    prop_gap_median = round(median(as.numeric(prop_p_gap)), 2),
  )


tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)




#'
#'
#'
#'
#'
#' ## Appendix - Tables of GIA Gap v.s. GIA Group by Platforms
#'
#' How GIA Gap distributed in each of GIA groups in each of those three
#' platforms, TRAC, OMNI and Reflex.
#'
#' GIA Groups are based on data on 12/31/2019
#'
#' ### TRAC
#'
#+ echo = F, warning = F

tab <- mydata_yrs_trac %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )


tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)

#'
#' ### OMNI
#'
#+ warning = F, echo = F 


tab <- mydata_yrs_omni %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )



tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)


#'
#' ### Reflex
#'
#+ warning = F, echo = F


tab <- mydata_yrs_reflex %>%
  ungroup() %>%
  filter(date == "12/31/2019") %>%
  group_by(gia_p_gp, gia_gap_gp) %>%
  summarise(
    total = n(),
    gia_p_avg = round(mean(gia_p), 2),
    gia_gap_avg = round(mean(as.numeric(gia_p_gap)), 2),
    gia_gap_median = round(median(as.numeric(gia_p_gap)), 2),
  )


tab %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_styling(full_width = F)
