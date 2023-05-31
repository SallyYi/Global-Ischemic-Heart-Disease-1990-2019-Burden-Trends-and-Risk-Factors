setwd("/Users/jiangshali/Desktop/GBD")
getwd()
library(tidyverse)

# 读取数据 --------------------------------------------------------------------

df <- read_csv("ischemic_heart_disease.csv")
colnames(df)
df <-df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                           "cause_name","metric_name","year","val","upper","lower")]
colnames(df) <- c("measure","location","sex","age",
                  "cause","metric","year","val","upper","lower")

number<-df|>
  filter(age=="All ages")|>
  filter(metric=="Number")|>
  filter(measure=="Incidence")
number$val <- as.numeric(number$val)
number$upper <- as.numeric(number$upper)
number$lower <- as.numeric(number$lower)
number <- number|>
  mutate(val=round(val,0),#将数据变为整数
         upper=round(upper,0),
         lower=round(lower,0))|>
  arrange(cause,sex,year)#重排序，非常重要
ASR<-df|>
  filter(age=="Age-standardized")|>
  filter(measure=="Incidence")|>
  filter(metric=="Rate")
ASR$val <- as.numeric(ASR$val)
ASR$upper <- as.numeric(ASR$upper)
ASR$lower <- as.numeric(ASR$lower)

unique(ASR$cause)
ASR <- ASR|>
  mutate(se=(upper-lower)/(2*1.96))|>#生成se
  arrange(cause,sex,year)#重排序，非常重要
ASR$year <- as.numeric(ASR$year)
ASR <- unique(ASR)
number$year <- as.numeric(number$year)
ASR<-ASR|>
  filter(location%in%c("Global","High-middle SDI","Low-middle SDI","Middle SDI",
                       "High SDI","Low SDI"))
ASR <- ASR|>
  arrange(cause,sex,year,location)

unique(ASR$location)
#写出数据
write.csv(number,"number.csv",row.names=F)
write.csv(ASR,"ASR.csv",row.names=F)
write.csv(Female_Age_rate_proj,"(Female_Age_rate_proj.csv",row.names = FALSE)
 write.csv(Male_Age_rate_proj,"Male_Age_rate_proj.csv",row.names = FALSE)
