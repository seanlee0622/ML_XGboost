---
title: "ML_template"
author: "Korean Data Team"
date: "2020-03-04"
output:
  html_document:
    theme: cerulean
    keep_md: true
    toc: true
    toc_width: 10
    toc_float: true
    code_folding: hide
---

<!-- 1.Objective -->
<!-- 2. EDA -->
<!-- 3. Feature Engineering -->
<!-- 4.Machine Learning -->
<!-- 5. Conclusion -->
<!-- Feature Engineering <- Feature Selection, Imputation, Visualization -->
<!-- EDA structure, type, missing portion -->
<!-- Feature Engineering Wranglization -->

# 1. Objective of analysis

### a. Use this packages.

```r
# Load the Packages
library(data.table)
library(janitor)
library(tidyverse)
library(DataExplorer)
library(naniar)
library(skimr)
library(plotly)
library(corrplot)
library(DT)
library(caret)
library(stringi)
library(lubridate)
library(gridExtra)
library(ggrepel)
library(caret)
library(corrplot)
library(rattle)

# Instant load the packages
pacman::p_load(data.table, janitor, tidyverse, DataExplorer, naniar, skimr, plotly, corrplot, DT, caret, stringi, lubridate, gridExtra, ggrepel, caret, corrplot, rattle)
```

### b. Load the data

```r
# Clean the name of variables (lower.case & '_' connection) by using Janitor package

# Method 1
data <- fread('url', stringsAsFactors = F) %>% clean_names()

# Method 2
data <- read.csv("url", header = TRUE) %>% clean_names()
```


# 2. EDA (Expolatory Data Analysis)

### a. Scales of data - Insert your data


```r
# ncol() | nrow()
cat("The 'data1' has", dim(data1)[1], "rows and", dim(data1)[2], "columns."
    "\n The 'data2' has",dim(data2)[1], "rows and", dim(data2)[2], "columns.")
```


### b. Type of variables 


```r
lapply(c(data1, data2), typeof) %>% data.frame() %>%  t() %>%  as.data.frame() 
```


### c. How many missing values for each dataset?


```r
# missing_transform
data1 <- sapply(data1, function(x) {ifelse(is.character(x) & nchar(x) == 0, NA, x)}) %>% data.frame(stringsAsFactors = F) # Careful to not use as.data.frame()
data2 <- sapply(data2, function(x) {ifelse(is.character(x) & nchar(x) == 0, NA, x)}) %>% data.frame(stringsAsFactors = F)
```


```r
# Show missing value - visualization
missing_data1 <- plot_missing(data1, missing_only = T, theme_config = list(legend.position = "right"), ggtheme = theme_light(), title = "data1 missing") %>% ggplotGrob()
missing_data2 <- plot_missing(data2, theme_config = list(legend.position = "right"), ggtheme = theme_light(), title = "data2 missing") %>% ggplotGrob()
```


```r
# Show missing result by using grid
grid.arrange(missing_data1, missing_data2, ncol = 1)
```

- The missing plot per variable can be created with $naniar$ Package as well below

- There is $gg\_miss\_var$ plot which indicates how much proportion of missing values in $nanair$ pacakge below. 


```r
# Show missing values by using naniar package
gmv1 <- naniar::gg_miss_var(data1, show_pct = T)
gmv2 <- naniar::gg_miss_var(data2, show_pct = T)
grid.arrange(gmv1, gmv2, nrow = 1)
```

- Also, it can be visualized with other way shown below. (Not recommended)


```r
# Show missing values by using naniar package (Not recommended)
m1 <- naniar::vis_miss(data1, warn_large_data = F) + labs(title = "data1 missing")
m2 <- naniar::vis_miss(data2, warn_large_data = F) + labs(title = "data2 missing")
grid.arrange(m1, m2, ncol = 1)
```


### d. Switching original continous features to any datatype(i.e. numeric, characater, date, ...) value


```r
# Using mutate_at
data1 <- data1 %>% mutate_at(colnames(data1) %>% grep("gpa|act|credits", ., value =T), as.numeric)
data2 <- data2 %>% mutate(total_meeting_duration = total_meeting_duration %>% as.numeric())

t1 <- data1 %>% select_if(is.numeric) %>% colnames() %>% data.frame() 
t2 <- data2 %>% select_if(is.numeric) %>% colnames() %>% data.frame()
rbind(t1, t2) %>% mutate(type = "Numeric") # Create 'type' column
```

### e. DataExplorer Package 


```r
# DataExplor package - numeric variable & categorical variable & row & column
t1 <- DataExplorer::introduce(data1) %>% t() %>% as.data.frame() %>% rownames_to_column("attribute")
t2 <- DataExplorer::introduce(data2) %>% t() %>% as.data.frame() %>% rownames_to_column("attribute")

# DataExplor package - Visualization
p1 <- DataExplorer::plot_intro(data1, title = "data1", ggtheme = theme_bw())
p2 <- DataExplorer::plot_intro(data2, title = "data2", ggtheme = theme_bw())
```


```r
# Combine tables and plots in a row.
grid.arrange(p1, tableGrob(t1, rows=NULL), tableGrob(t2, rows=NULL), p2,  ncol = 2, top = "Attributes of datasets")
```



# 3. Feature Engineering (Feature selection, Imputation, Visualization)

### a. data1 & data2 Joining key


```r
joining_key <- data1[,which(colnames(data1) %in% colnames(data2))] %>% colnames()

join_data <- 
  left_join(data2, data1, by = joining_key)

# datatable(join_data %>% head(100), options = list(scrollX = T))
```

### b. 'target column' (Target)

__Before__

```r
join_data %>% group_by('target column') %>% tally() 
```

__After__

```r
join_data <- join_data %>% mutate('target column' = ifelse(str_detect('target column',"Graduated"), "Graduated", "Not Graduated"))

join_data$'target column' %>% unique()
```




# 4. Machine Learning


## 1. Target Distribution


```r
proportion <- advising_student_final %>% group_by(enrollment_status) %>% count() %>% ungroup() %>%  mutate(pct = round(n/sum(n),2))
proportion
```


## 2. Identify the missing variables and impute the part (median)


```r
naniar::gg_miss_var(advising_student_final, show_pct = T)
# naniar::gg_miss_fct(advising_student_final, enrollment_status)

# advising_student_final %>% ggplot() +
#   geom_histogram(aes(x = act_composite, fill = enrollment_status)) +
#   facet_wrap(~ enrollment_status) +
#   theme_minimal()
# 
# advising_student_final %>% ggplot() +
#   geom_histogram(aes(x = high_school_gpa, fill = enrollment_status)) +
#   facet_wrap(~ enrollment_status) +
#   theme_minimal()

advising_student_final %>% group_by(enrollment_status) %>% summarise(act_median = median(act_composite, na.rm = T), hs_gpa_median = median(high_school_gpa, na.rm = T))

advising_student_final[advising_student_final$enrollment_status == "Graduated" & is.na(advising_student_final$act_composite),"act_composite"] <- 22
advising_student_final[advising_student_final$enrollment_status == "Not Graduated" & is.na(advising_student_final$act_composite),"act_composite"] <- 22
advising_student_final[advising_student_final$enrollment_status == "Graduated" & is.na(advising_student_final$high_school_gpa),"high_school_gpa"] <- 3.46
advising_student_final[advising_student_final$enrollment_status == "Not Graduated" & is.na(advising_student_final$high_school_gpa),"high_school_gpa"] <- 3.24


advising_student_final <- advising_student_final %>% mutate_if(is.character, as.factor) %>% mutate_if(is.logical, as.factor) %>% select(-c(identifier))

sapply(advising_student_final, class)

naniar::gg_miss_var(advising_student_final, show_pct = T)
```


```r
set.seed(1000)
InTraining <- caret::createDataPartition(advising_student_final$enrollment_status, p = .7, list = F)
Train <- advising_student_final %>% slice(InTraining)
Test <- advising_student_final %>% slice(-InTraining)

ModelFit_1 <- train(enrollment_status ~ ., data = Train, method = "rf", importance = T)
ModelFit_2 <- train(enrollment_status ~ ., data = Train, method = "rpart")
```


```r
varimp1 <- ggplot(varImp(ModelFit_1)) + geom_label(aes(label = paste0(round(Importance),"%"))) + theme_bw() + labs(title = "Random Forest") + theme(plot.title = element_text(hjust = 0.5))
varimp2 <- ggplot(varImp(ModelFit_1)) + geom_label(aes(label = paste0(round(Importance),"%"))) + theme_bw() + labs(title = "RPart") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(varimp1, varimp2, ncol = 1)
```


```r
caret::confusionMatrix(Test$enrollment_status ,predict(ModelFit_1, Test))
caret::confusionMatrix(Test$enrollment_status ,predict(ModelFit_2, Test))
```



```r
fancyRpartPlot(ModelFit_2$finalModel)
```


```r
colSums(is.na(advising_student_final))

advising_student_final <- advising_student_final %>% mutate(pred = predict(ModelFit_1, advising_student_final))

confusionMatrix(advising_student_final$enrollment_status, advising_student_final$pred)
```









# * Useful Code *

### round(), percentage


```r
table(advising_student_fs$enrollment_status) %>% data.frame() %>% mutate(Pct = round(Freq / sum(Freq) * 100, 2))
```


### ggplot help


```r
t1 <- advising_student_fs %>% 
  group_by(enrollment_status) %>% 
  summarise(mean = mean(sum_of_cumulative_missing_credit, na.rm = T)) 

advising_student_fs %>% 
  ggplot(aes(x = enrollment_status, y = sum_of_cumulative_missing_credit)) +
  geom_jitter(alpha = 0.3, color = "gray") +
  geom_violin(aes(fill = enrollment_status)) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  annotation_custom(tableGrob(t1, rows = NULL), xmin= 0.5, xmax=  1 , ymin= 50 , ymax= 60)
```


### Lubridate::%--% = as.interval()

##### Ex1

```r
fix_year <- function(x, year=1919){ #since this year is 2019, if year digits are over 19, it is regared as 1900 century.
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

advising_student_fs <-
advising_student_fs %>% 
  mutate(mission_enter_date = mdy(mission_enter_date) %>% fix_year(), mission_release_date = mdy(mission_release_date) %>% fix_year()) %>% 
  mutate(served_mission = mission_enter_date %--% mission_release_date %>% as.duration()/ ddays(1)) %>% 
  mutate(served_mission = case_when(
    is.na(served_mission) ~ "N",
    served_mission < 100 ~ "H",
    TRUE ~ "F"
  ))
```

##### Ex2

```r
advising_student_fs <- 
advising_student_fs %>% 
  mutate(age = round(mdy(birth_date) %>% fix_year() %--% ymd(paste0(20,parse_number(term_code)), truncated = 2L) %>% as.duration() / dyears(1)))
```



################################################

## - Subprogram (Dayschool VS Online)


```r
advising_student_fs <-
advising_student_fs %>% 
  mutate(subprogram = substring(subprogram,1,1))
```

- Students can be classified into in-school student (D) or online student. (O)

## - Check if student transferred from other institution


```r
advising_student_fs <-
advising_student_fs %>% 
  mutate(transfer = ifelse(transfer_earned_credits > 0, 1, 0) %>% as.factor())

advising_student_fs %>% group_by(transfer) %>% count()
```


## - Top 5 Major by level(degree) 


```r
major_fs <- advising_student_fs %>% group_by(level, major) %>% tally(sort = T)

major_fs %>% group_by(level) %>% top_n(5) %>% ungroup() %>% 
  ggplot(aes(x = parse_number(major) %>% as.factor() %>% reorder(n), y = n)) +
  geom_col(aes(fill = level)) +
  geom_label(aes(label = str_extract(major, "[:alpha:]+"))) +
  facet_wrap(level ~., scales = "free", ncol = 1) +
  coord_flip() +
  theme_bw() +
  labs(x = "Major", y = "Count", title = "Top 5 Major by degree") +
  scale_fill_discrete(label = c("Assocaite", "Bachelor"))
```

## - Top 5 Advising Reason by level(degree)


```r
advising_reason_fs <-
  advising_student_fs %>% group_by(level, advising_reason, enrollment_status) %>% count() %>% ungroup() %>% group_by(level, enrollment_status) %>% top_n(5, n)

advising_reason_fs %>% ggplot(aes(x = reorder(advising_reason, -n), y = n)) +
  geom_col(aes(fill = level), position = "dodge") +
  facet_wrap( level ~ enrollment_status, scales = "free") +
  theme_light() +
  coord_flip() +
  theme(legend.position = "top") +
  labs(x = "", y = "") +
  scale_fill_discrete(labels = c("Associate","Bachelor")) +
  labs(caption = "We can see that Graduated Students are more likely to visit advising center with the reason of Glad Planner & Class Planning compared to others")
```


## - Aggregate the data group by student id (Identifier)

After feature engineering, I aggregated the data by distinct Identifier to manage the individual data of each student.


```r
advising_student_final <- 
advising_student_fs %>% 
  group_by(identifier) %>% 
  arrange(cumulative_earned_credits) %>% 
  summarise(total_meeting_duration = sum(total_meeting_duration),
            number_of_semester = length(enrolled_current_semester[unique(term_code)] %>% str_detect("T")) ,
            classification_begin = head(classification_begin,1),
            served_mission = head(served_mission,1),
            subprogram = head(subprogram,1),
            gender = head(gender, 1),
            act_composite = mean(act_composite),
            high_school_gpa = mean(high_school_gpa),
            level = head(level, 1),
            cumulative_gpa = tail(cumulative_gpa, 1),
            transfer = head(transfer, 1),
            age = round(mean(age)),
            cumulative_earned_credits = head(cumulative_earned_credits, 1),
            cumulative_attempted_credits = head(cumulative_attempted_credits, 1),
            cumulative_missing_credit = round(max(cumulative_attempted_credits) - (max(cumulative_earned_credits) - head(transfer_earned_credits,1)),2),
            cumulative_campus_earned_credit = max(cumulative_earned_credits) - head(transfer_earned_credits,1),
            transfer_earned_credits = mean(transfer_earned_credits, na.rm = T),
            # target variable
            enrollment_status = head(enrollment_status, 1)) %>% 
  ungroup()

# datatable(advising_student_final,)
```




