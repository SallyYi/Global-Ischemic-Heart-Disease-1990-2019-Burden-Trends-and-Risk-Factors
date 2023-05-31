rm(list = ls())
setwd("/Users/jiangshali/Desktop/GBD")
getwd()
install.packages("remotes")
library(remotes)
remotes::install_github("haraldwf/nordpred")
library(nordpred)
library(data.table)
library(tidyverse)
library(ggplot2)
install.packages("epitools")
install.packages("reshape2")
library(epitools)
library(reshape2)
source('function_sum_year.R')
EC <-  read_csv('ischemic_heart_disease.csv')
age_stand <- read_csv('GBD2019 world population age standard.csv')[-c(1:3),]
colnames(EC)
EC <- EC[,colnames(EC)%in%c("measure_name","location_name","sex_name","age_name",
                               "cause_name","metric_name","year","val","upper","lower")]
colnames(EC) <- c("measure","location","sex","age",
                  "cause","metric","year","val","upper","lower")
head(EC)
rownames(age_stand) <- 1:nrow(age_stand)
knitr::kable(age_stand,digits=2,align = 'c')
unique(EC$age)
EC <- EC %>% mutate(age=sub('-',replacement = ' to ', age)) %>% 
  mutate(age=sub(' years',replacement = '', age)) %>% 
  mutate(age=sub(' year',replacement = '', age)) %>% 
  mutate(age=sub('95\\+',replacement = '95 plus', age)) %>% 
  mutate(age=sub('Age to standardized',replacement = 'Age-standardized', age)) %>% 
  mutate(age=sub('<5',replacement = 'Under 5', age)) %>% 
  filter(val>0)
unique(EC$age)
#### 疾病真实的年龄结构
ages <- c("20 to 24", "25 to 29",
          "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
          "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
          "90 to 94", "95 plus")

#### 调取标准人口百分比用
ages_2 <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")

####  预测的年龄结构
ages_3 <- c("0 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",  "85 to 89",
            "90 to 94", "95 plus")

wstand <- c(age_stand$std_population[1:4] %>% as.numeric() %>% sum(),
            age_stand$std_population[5:21])/sum(age_stand$std_population[1:21])
wstand
### for incidence for Male and female
EC_Male_incidence <- EC %>% filter(age %in% ages &
                                     sex == 'Male' &
                                     metric == 'Number' &
                                     measure == 'Incidence' &
                                     location== 'Global')

EC_Male_incidence_n <- dcast(data = EC_Male_incidence, age~year, value.var = "val")

##### 扩展数据，将其年龄扩展成18个年龄段
EC_Male_incidence_n['0 to 14',] <- c('0 to 14',rep(0,ncol(EC_Male_incidence_n)-1))
EC_Male_incidence_n['15 to 19',] <- c('15 to 19',rep(0,ncol(EC_Male_incidence_n)-1))
EC_Male_incidence_n <- EC_Male_incidence_n %>% 
  mutate(age=factor(age,levels = ages_3,ordered = T)) %>% 
  arrange(age)

rownames(EC_Male_incidence_n) <- EC_Male_incidence_n$age
EC_Male_incidence_n <- EC_Male_incidence_n[,-1]
EC_Male_incidence_n <- apply(EC_Male_incidence_n, c(1,2), as.numeric) %>% as.data.frame()

knitr::kable(EC_Male_incidence_n,digits=2,align = 'c')

EC_Female_incidence <- EC %>% filter(age %in% ages &
                                       sex == 'Female' &
                                       metric == 'Number' &
                                       measure == 'Incidence' &
                                       location== 'Global')

EC_Female_incidence_n <- dcast(data = EC_Female_incidence, age~year, value.var = "val")
##### 扩展数据，将其年龄扩展成18个年龄段
EC_Female_incidence_n['0 to 14',] <- c('0 to 14',rep(0,ncol(EC_Female_incidence_n)-1))
EC_Female_incidence_n['15 to 19',] <- c('15 to 19',rep(0,ncol(EC_Female_incidence_n)-1))
EC_Female_incidence_n <- EC_Female_incidence_n %>% 
  mutate(age=factor(age,levels = ages_3,ordered = T)) %>% 
  arrange(age)

rownames(EC_Female_incidence_n) <- EC_Female_incidence_n$age
EC_Female_incidence_n <- EC_Female_incidence_n[,-1]
EC_Female_incidence_n <- apply(EC_Female_incidence_n, c(1,2), as.numeric) %>% as.data.frame()
head(EC_Female_incidence_n)

##### 读取人口学数据
dirname <- dir("GBD_Population")
file <- paste0(getwd(),"/GBD_Population/",dirname)
var_name <- c('location_id',"location_name","sex_name","year_id","age_group_name","val")

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name)))
names(GBD_population)=var_name
for (a in file) {
  data <- fread(a) %>% as.data.frame() %>% select(var_name) %>%
    filter(age_group_name %in% ages_2 & location_id !=533)
  GBD_population <- rbind(GBD_population,data)
}
GBD_population <- GBD_population %>% mutate(sex_name = case_when(
  sex_name == "both"  ~ "Both",
  sex_name == "male"  ~ "Male",
  sex_name == "female" ~ "Female")) %>% 
  select(-1)
head(GBD_population)

#### 读取预测人口数据
prediction_var_name <- c("location_name","sex","year_id","age_group_name","val")
GBD_population_prediction <- fread('IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv') %>% as.data.frame() %>% 
  select(prediction_var_name) %>%
  filter(year_id %in% 2020:2044)

names(GBD_population_prediction) <- var_name[-1]

unique(GBD_population_prediction$age_group_name)

GBD_1year <- GBD_population_prediction %>% 
  filter(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal")) %>%
  group_by(location_name,sex_name,year_id) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_group_name="<1 year") %>% 
  select(var_name[-1])

GBD_population_prediction <- GBD_population_prediction %>% filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal"))) %>%
  rbind(GBD_1year)
unique(GBD_population_prediction$age_group_name)

##### 合并现人口数+预测人口数
GBD <- rbind(GBD_population,GBD_population_prediction)
#### 合并0-14
GBD_age14 <- GBD %>% filter(age_group_name %in% c("<1 year","1 to 4","5 to 9","10 to 14")) %>%
  group_by(location_name,sex_name,year_id) %>%
  summarize(val=sum(val)) %>%  mutate(age_group_name='0 to 14') %>% select(c(1:3,5,4))
GBD <- rbind(GBD,GBD_age14)
GBD <- GBD %>% filter(age_group_name %in% ages_3) %>%
  mutate(age_group_name=factor(age_group_name,levels=ages_3,ordered = T)) %>%
  arrange(age_group_name)
unique(GBD$age_group_name)

##  提取对应国家的人口学数据
GBD_Global_Male <- GBD %>% filter(location_name=='Global' & sex_name == 'Male')
GBD_Global_Female <-  GBD %>% filter(location_name=='Global' & sex_name == 'Female')

GBD_Global_Male_n <- dcast(data = GBD_Global_Male, 
                           age_group_name ~ year_id,
                           value.var = c("val")) %>% 
  select(-1)
GBD_Global_Female_n <- dcast(data = GBD_Global_Female, 
                             age_group_name ~ year_id,
                             value.var = c("val")) %>% 
  select(-1)

GBD_Global_Male_n <- apply(GBD_Global_Male_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_Female_n <- apply(GBD_Global_Female_n, c(1,2), as.numeric) %>% as.data.frame()
EC_Male_incidence_n <- apply(EC_Male_incidence_n, c(1,2), as.numeric) %>% as.data.frame()
EC_Female_incidence_n <- apply(EC_Female_incidence_n, c(1,2), as.numeric) %>% as.data.frame()

EC_Male_incidence_g <- function_sum_year5(EC_Male_incidence_n,1990,2019,2019)
EC_Female_incidence_g <- function_sum_year5(EC_Female_incidence_n,1990,2019,2019)
GBD_Global_Male_g <- function_sum_year5(GBD_Global_Male_n,1990,2044,2019)
GBD_Global_Female_g <- function_sum_year5(GBD_Global_Female_n,1990,2044,2019)

rownames(EC_Male_incidence_g) <- ages_3
rownames(EC_Female_incidence_g) <-  ages_3
rownames(GBD_Global_Female_g) <-  ages_3
rownames(GBD_Global_Male_g) <-  ages_3

GBD_Global_Male_g <- apply(GBD_Global_Male_g, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_Female_g <- apply(GBD_Global_Female_g, c(1,2), as.numeric) %>% as.data.frame()
EC_Male_incidence_g <- apply(EC_Male_incidence_g, c(1,2), as.numeric) %>% as.data.frame()
EC_Female_incidence_g <- apply(EC_Female_incidence_g, c(1,2), as.numeric) %>% as.data.frame()

Male_res <- nordpred(EC_Male_incidence_g, GBD_Global_Male_g,
                     noperiods = 4:6, startestage = 3, startuseage = 3,
                     cuttrend = c(0, .25, .5, .75, .75), linkfunc = "power5",                           recent = NULL)

Female_res <- nordpred(EC_Female_incidence_g, GBD_Global_Female_g,
                       noperiods = 4:6, startestage = 3, startuseage = 3,
                       cuttrend = c(0, .25, .5, .75, .75), linkfunc = "power5",                           recent = NULL)

## 不同年龄段的发病率
round(nordpred.getpred(Male_res, incidence = TRUE, standpop = NULL), 2)

## 标准发病率
round(nordpred.getpred(Male_res, incidence = TRUE, standpop = wstand), 2)

## 不同年龄段的发病数
round(nordpred.getpred(Male_res, incidence = FALSE, standpop = NULL), 2)

## producing the end periods rates:
Male_Age_rate <- nordpred.getpred(Male_res, incidence = TRUE, standpop = NULL)
## 提取预测的率的数据
Male_Age_rate_proj <- Male_Age_rate[,7:ncol(Male_Age_rate)]
head(Male_Age_rate_proj)

## 提取头之前之前的数据
rc1 <- Male_Age_rate[, 6]
rc1

## 计算尾部+1的数据
rc2 <- 2*Male_Age_rate_proj[, ncol(Male_Age_rate_proj)] - Male_Age_rate_proj[, (ncol(Male_Age_rate_proj)-1)]
rc2

##合并预测的数据
full_rate_proj <- cbind(rc1, Male_Age_rate_proj, rc2)
head(full_rate_proj)

annual_Male_Age_rate_proj <- matrix(NA, 18, 5*ncol(Male_Age_rate_proj))
for (i in 2:(5+1)){
  annual_Male_Age_rate_proj[,(i-2)*5+1] <- (2/5)*full_rate_proj[,i-1] + (3/5)*full_rate_proj[,i]
  annual_Male_Age_rate_proj[,(i-2)*5+2] <- (1/5)*full_rate_proj[,i-1] + (4/5)*full_rate_proj[,i]
  annual_Male_Age_rate_proj[,(i-2)*5+3] <- (0/5)*full_rate_proj[,i-1] + (5/5)*full_rate_proj[,i]
  annual_Male_Age_rate_proj[,(i-2)*5+4] <- (1/5)*full_rate_proj[,i+1] + (4/5)*full_rate_proj[,i]
  annual_Male_Age_rate_proj[,(i-2)*5+5] <- (2/5)*full_rate_proj[,i+1] + (3/5)*full_rate_proj[,i]
}
annual_Male_Age_rate_proj <- annual_Male_Age_rate_proj %>% as.data.frame()
names(annual_Male_Age_rate_proj) <- 2020:(2020+ncol(annual_Male_Age_rate_proj)-1)
rownames(annual_Male_Age_rate_proj) <- ages_3

knitr::kable(annual_Male_Age_rate_proj,digits=2,align = 'c')

annual_Male_Age_rate_ob <- EC_Male_incidence_n/GBD_Global_Male_n[,1:30]*100000

Male_Age_rate <- cbind(annual_Male_Age_rate_ob,annual_Male_Age_rate_proj)
Male_Age_count <- Male_Age_rate*GBD_Global_Male_n/100000
rownames(Male_Age_rate) <- ages_3
rownames(Male_Age_count) <- ages_3
## 每个年龄段的发病率
head(Male_Age_rate)

## 每个年龄段的发病数
head(Male_Age_count)

## 总发病数
Male_sum_year <- apply(Male_Age_count, 2, sum) %>% as.data.frame()
Male_sum_year$year <- 1990:(1990+nrow(Male_sum_year)-1)
names(Male_sum_year) <- c("number","year")
head(Male_sum_year)

## 计算标准发病率
Male_Age_standardized <- matrix(nrow =0, ncol = 2) %>% as.data.frame()
names(Male_Age_standardized) <- c("ASR","year")

for (i in 1:ncol(Male_Age_count)) {
  asr = ageadjust.direct(count = Male_Age_count[,i], pop = GBD_Global_Male_n[,i],
                         stdpop = wstand)
  Male_Age_standardized[i,1:2] <- c(round(100000*asr, 2)[2],names(Male_Age_count)[i]) ##rate per 100,000 per year
}
head(Male_Age_standardized)

Female_Age_rate <- nordpred.getpred(Female_res, incidence = TRUE, standpop = NULL)
Female_Age_rate_proj <- Female_Age_rate[,7:ncol(Female_Age_rate)]
rc1 <- Female_Age_rate[, 6]
rc2 <- 2*Female_Age_rate_proj[, ncol(Female_Age_rate_proj)] - Female_Age_rate_proj[, (ncol(Female_Age_rate_proj)-1)]
full_rate_proj <- cbind(rc1, Female_Age_rate_proj, rc2)

# producing annual age-specific rates:
annual_Female_Age_rate_proj <- matrix(NA, 18, 5*ncol(Female_Age_rate_proj))
for (i in 2:(5+1)){
  annual_Female_Age_rate_proj[,(i-2)*5+1] <- (2/5)*full_rate_proj[,i-1] + (3/5)*full_rate_proj[,i]
  annual_Female_Age_rate_proj[,(i-2)*5+2] <- (1/5)*full_rate_proj[,i-1] + (4/5)*full_rate_proj[,i]
  annual_Female_Age_rate_proj[,(i-2)*5+3] <- (0/5)*full_rate_proj[,i-1] + (5/5)*full_rate_proj[,i]
  annual_Female_Age_rate_proj[,(i-2)*5+4] <- (1/5)*full_rate_proj[,i+1] + (4/5)*full_rate_proj[,i]
  annual_Female_Age_rate_proj[,(i-2)*5+5] <- (2/5)*full_rate_proj[,i+1] + (3/5)*full_rate_proj[,i]
}
annual_Female_Age_rate_proj <- annual_Female_Age_rate_proj %>% as.data.frame()
names(annual_Female_Age_rate_proj) <- 2020:(2020+ncol(annual_Female_Age_rate_proj)-1)
rownames(annual_Female_Age_rate_proj) <- ages_3

annual_Female_Age_rate_ob <- EC_Female_incidence_n/GBD_Global_Female_n[,1:30]*100000

Female_Age_rate <- cbind(annual_Female_Age_rate_ob,annual_Female_Age_rate_proj)
Female_Age_count <- Female_Age_rate*GBD_Global_Female_n/100000
rownames(Female_Age_rate) <- ages_3
rownames(Female_Age_count) <- ages_3


Female_sum_year <- apply(Female_Age_count, 2, sum) %>% as.data.frame()
Female_sum_year$year <- 1990:(1990+nrow(Female_sum_year)-1)
names(Female_sum_year) <- c("number","year")


Female_Age_standardized <- matrix(nrow =0, ncol = 2) %>% as.data.frame()
names(Female_Age_standardized) <- c("ASR","year")

for (i in 1:ncol(Female_Age_count)) {
  asr = ageadjust.direct(count = Female_Age_count[,i], pop = GBD_Global_Female_n[,i],
                         stdpop = wstand)
  Female_Age_standardized[i,1:2] <- c(round(100000*asr, 2)[2],names(Female_Age_count)[i]) ##rate per 100,000 per year
}

head(Female_Age_rate)

head(Female_Age_count)
head(Female_sum_year)
head(Female_Age_standardized)
GBD_Global_Both_n <- GBD_Global_Female_n + GBD_Global_Male_n
Both_Age_count <- Female_Age_count + Male_Age_count
Both_Age_rate <- Both_Age_count/GBD_Global_Both_n*10^5

Both_Age_standardized <- matrix(nrow =0, ncol = 2) %>% as.data.frame()
names(Both_Age_standardized) <- c("ASR","year")

for (i in 1:ncol(Both_Age_count)) {
  asr = ageadjust.direct(count = Both_Age_count[,i], pop = GBD_Global_Both_n[,i],
                         stdpop = wstand)
  Both_Age_standardized[i,1:2] <- c(round(100000*asr, 2)[2],names(Both_Age_count)[i]) ##rate per 100,000 per year
}

Both_sum_year <- (Male_sum_year[,1] + Female_sum_year[,1]) %>% as.data.frame()
colnames(Both_sum_year) <- 'number'
Both_sum_year$year <- rownames(Female_sum_year)


head(Both_Age_rate)
head(Both_Age_count)
head(Both_sum_year)
head(Both_Age_standardized)

####### plot for prediction
Both_sum_year$sex <- 'Both'
Female_sum_year$sex <- 'Female'
Male_sum_year$sex <- 'Male'

Both_Age_standardized$sex <- 'Both'
Female_Age_standardized$sex <- 'Female'
Male_Age_standardized$sex <- 'Male'

ASR <- rbind(Both_Age_standardized,Female_Age_standardized,Male_Age_standardized)
Num <- rbind(Both_sum_year,Female_sum_year,Male_sum_year)
ASR$ASR <- as.numeric(ASR$ASR)
ASR$year <- as.numeric(ASR$year)
Num$number <- as.numeric(Num$number)
Num$year <- as.numeric(Num$year)

ratio <-max(ASR$ASR)/max(Num$number)

p <- ggplot(Num,aes(year,number))+
  geom_col(aes(fill=sex),position = 'dodge',width = 0.8)+
  labs(title = NULL,x='Year',y='Number of cases') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45,size=8,color='black'),
        axis.text.y=element_text(size=8,color='black'),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        strip.background.x = element_rect(fill = 'skyblue3'),
        title = element_text(size = 10, hjust = 0.5),
        legend.position = 'right') +
  geom_line(data=ASR,
            aes(x=year,y=ASR/ratio,
                color=sex)) + 
  scale_x_continuous(expand=c(0,0))
scale_y_continuous(expand=c(0,0),sec.axis = sec_axis(~.*ratio,
                                                     name="Age-standardized rate (per 100000 populations)"))
p
ggsave("Figure 14.pdf",width=11.79,height=8,units="in",dpi=300)
write.csv(Both_Age_count,"Both_Age_count.csv",row.names = FALSE)
write.csv(Both_Age_rate,"Both_Age_rate.csv",row.names = FALSE)
write.csv(Num,"Num.csv",row.names = FALSE)
write.csv(ASR,"ASR.csv",row.names = FALSE)
Male_sum_year
write.csv(Female_Age_rate_proj,"Female_Age_rate_proj.csv",row.names = T)
write.csv(Male_Age_rate_proj,"Male_Age_rate_proj.csv",row.names =T)
