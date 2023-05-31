# 数据筛选脚本, for joinpoint
# 2022-11-19

library(tidyverse)

# 设定路径
getwd()

df <- read_csv("ischemic_heart_disease.csv")
df <- df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                            "cause_name","metric_name","year","val","upper","lower")]
colnames(df) <- c("measure","location","sex","age",
                  "cause","metric","year","val","upper","lower")

df <- df|>
  filter (location %in% c("Global","High SDI","High-middle SDI",
                         "Middle SDI","Low-middle SDI","Low SDI"))
unique(df$location)

df<-na.omit(df)
str(df)
df$year <- as.numeric(df$year)
df$val <- as.numeric(df$val)
df$lower <- as.numeric(df$lower)
colnames(df)
unique(df$measure)
unique(df$location)
unique(df$sex)
unique(df$age)
unique(df$cause)
unique(df$metric)
unique(df$year)

# 筛选数据：
number <- df |> 
  filter(age=="All ages") |> 
  filter(metric=="Number") |> 
  filter(measure=="Incidence") |> 
  mutate(val=round(val,0),    #将数据变为整数
         upper=round(upper,0),
         lower=round(lower,0)) |> 
  arrange(cause,sex,location,year)    #重排序，非常重要


ASR <- df |> 
  filter(age=="Age-standardized") |> 
  filter(measure=="Incidence") |> 
  filter(metric=="Rate") |> 
  mutate(se=(upper-lower)/(2*1.96)) |>  #生成se
  arrange(cause,location,sex,year)    #重排序，非常重要

# 写出数据
write.csv(number,"number.csv",row.names = F)
write.csv(ASR,"ASR.csv",row.names = F)





