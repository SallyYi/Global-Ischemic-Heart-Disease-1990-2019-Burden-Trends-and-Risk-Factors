install.packages("MetBrewer")
library(tidyverse)
library(data.table)
rm(Data=df)
# 批量读取人口数据 ----------------------------------------------------------------

dir <- "/Users/jiangshali/Desktop/人口数据"
#获得文件列表
files<-list.files(dir,#路径参数
             pattern=".CSV",#后缀为.CSV的都读取,这里为什么要大写？
             full.names=T)
#批量读取到一个文件中
pop<-map_dfr(files,read.csv)
#
查看数据
colnames(pop)
pop<-pop%>%
  select(location_name,sex_name,age_group_name,year_id,metric_name,val)
colnames(pop)<-c("location","sex","age","year","metric","val")
pop$sex[pop$sex=="male"]<-"Male"#疾病数据是大写开头的
pop$sex[pop$sex=="female"]<-"Female"
pop$sex[pop$sex=="both"]<-"Both"

# 读取疾病数据 ------------------------------------------------------------------

setwd("/Users/jiangshali/Desktop/GBD")
getwd()
#读取数据,选择Global 
disease<-read_csv("ischemic_heart_disease.csv")
colnames(disease)
disease <- disease|>
  select( "measure_name","location_name","sex_name","age_name","cause_name",   
          "metric_name","year","val","upper","lower")
colnames(disease) <- c("measure","location","sex","age","cause",   
                       "metric","year","val","upper","lower")
unique(disease$age)
#查看变量的取值范围
#unique(disease$location)
#unique(disease$metric)
unique(disease$age)
disease<-disease|>
  filter(location=="Global")|>
  filter(metric=="Number")|>
  filter(age %in% c("1-4 years","5-9 years","10-14 years","15-19 years",
                    "20-24 years","25-29 years","30-34 years",    
                     "35-39 years","40-44 years","45-49 years","50-54 years",     
                     "55-59 years","60-64 years","65-69 years","70-74 years",     
                     "75-79 years","80-84","85-89","90-94","95+ years"))|>
  mutate(across("age",str_replace,"1-4 years","1 to 4")) %>% 
  mutate(across("age",str_replace,"5-9 years","5 to 9")) %>% 
  mutate(across("age",str_replace,"10-14 years","10 to 14")) %>% 
  mutate(across("age",str_replace,"15-19 years","15 to 19")) %>% 
  mutate(across("age",str_replace,"20-24 years","20 to 24")) %>% 
  mutate(across("age",str_replace,"25-29 years","25 to 29")) %>% 
  mutate(across("age",str_replace,"30-34 years","30 to 34")) %>% 
  mutate(across("age",str_replace,"35-39 years","35 to 39")) %>% 
  mutate(across("age",str_replace,"40-44 years","40 to 44")) %>% 
  mutate(across("age",str_replace,"45-49 years","45 to 49")) %>% 
  mutate(across("age",str_replace,"50-54 years","50 to 54")) %>% 
  mutate(across("age",str_replace,"55-59 years","55 to 59")) %>%
  mutate(across("age",str_replace,"60-64 years","60 to 64")) %>% 
  mutate(across("age",str_replace,"65-69 years","65 to 69")) %>% 
  mutate(across("age",str_replace,"70-74 years","70 to 74")) %>% 
  mutate(across("age",str_replace,"75-79 years","75 to 79")) %>% 
  mutate(across("age",str_replace,"80-84","80 to 84")) %>% 
  mutate(across("age",str_replace,"85-89","85 to 89")) %>% 
  mutate(across("age",str_replace,"90-94","90 to 94")) %>% 
  mutate(across("age",str_replace,"95+ years","95 plus"))
disease <- disease|>
  mutate(across("age",str_replace,"95+ years","95 plus"))
unique(pop$age)
pop <- pop|>
  mutate(across("age",str_replace,"95 plus","95+ years"))
#这里选择各年龄组，不要allages

# 合并人口和疾病数据 ---------------------------------------------------------------

#使用left_joint函数合并疾病与人口数据
pop$year <- as.character(pop$year)
unique(disease$year)
unique(pop$year)
df<-left_join(disease,pop,by=c("location","sex","age","metric","year"))
#修改一下表头
colnames(df)[8]<-"case"
#这里的序号[8]根据数据来取
colnames(df)[11]<- "population"
df$case <- as.numeric(df$case)
df<-df%>%#将人口与患病人数都转换为整数
  mutate(population=round(population,digits=0))%>%
  mutate(case=round(case,digits=0))%>%#有些变量用不上，删除掉
  select(location,sex,age,cause,year,case,population)%>%#排个序
  arrange(location,sex,age,cause,year)
df<-df%>%#这里面患病人数为0的年龄组都删除了
filter(case!=0)

# 生成年龄，时期，队列变量 ------------------------------------------------------------

#将字符串的年龄转化为数值类型,并生成period,cohort
df$year <- as.numeric(df$year)
df<-df|>
  mutate(age2=age)|>
  mutate(age2=ifelse(age2=="95+ years","95 to 99",age2))|>
  separate(age2,c("age2","other"),"to")|>
  mutate(age2=as.numeric(age2))|>
  select(-other)|>
  rename(period=year)|>
  mutate(cohort=period-age2)|>
  #生成年龄、时期的标签
  rename(age_group=age)|>
  rename(age=age2)|>
  mutate(period_group=case_when(
    period<1995~"1990~94",
    period<2000~"1995~99",
    period<2005~"2000~04",
    period<2010~"2005~09",
    period<2015~"2010~14",
    period==2015~"2015~19"))


# 求各组的平均值 -----------------------------------------------------------------

#对每个分组求均值
df<-df|>
  group_by(location,sex,cause,age_group,period_group)|>
  mutate(case=round(mean(case),digits=0))|>
  mutate(population=round(mean(population),digits=0))|>
  filter(period%in%seq(1990,2019,by=5))|>
  #生成队列标签
  mutate(cohort_group=case_when(cohort==1895~"1895~1899",
                                cohort==1900~"1900~1904",
                                cohort==1905~"1905~1909",
                                cohort==1910~"1910~1914",
                                cohort==1915~"1915~1919",
                                cohort==1920~"1920~1924",
                                cohort==1925~"1925~1929",
                                cohort==1930~"1930~1934",
                                cohort==1935~"1935~1939",
                                cohort==1940~"1940~1944",
                                cohort==1945~"1945~1949",
                                cohort==1950~"1950~1954",
                                cohort==1955~"1955~1959",
                                cohort==1960~"1960~1964",
                                cohort==1965~"1965~1969",
                                cohort==1970~"1970~1974",
                                cohort==1975~"1975~1979",
                                cohort==1980~"1980~1984",
                                cohort==1985~"1985~1989",
                                cohort==1990~"1990~1994",
                                cohort==1995~"1995~1999",
                                cohort==2000~"2000~2004"))|>
  select(location,cause,sex,
         age_group,period_group,cohort_group,
         age,period,cohort,case,population)

# 写出数据 --------------------------------------------------------------------

write.csv(df,"data_for_apc.csv",row.names=F)

# 数据的展示 -------------------------------------------------------------------

rm(list=ls())
mydata<-read.csv("data_for_apc.csv",header=T)
#将分类变量设置为因子
mydata$age_group<-factor(mydata$age_group)
mydata$period_group<-factor(mydata$period_group)
mydata$cohort_group<-factor(mydata$cohort_group)
#查看数据，看看顺序有没有错
#levels(mydata$age_group)
#levels(mydata$period_group)
#levels(mydata$cohort_group)
#colnames(mydata)
mydata<-mydata|>
  arrange(age,period,cohort)%>%
  mutate(rate=(case/population)*100000)

# 画图 ----------------------------------------------------------------------

#age base variation of DM2 prevalence, by period
p1<-mydata%>%
  filter(sex=="Both")|>
  ggplot(aes(x=age_group,y=rate,group=period_group,color=period_group))+
  geom_line(size=1)+
  geom_point(shape=2)+
  guides(color=guide_legend(title="period"))+
  scale_color_manual(values=heat.colors(6))+
  scale_shape_discrete(guide="none")+
  xlab("")+
  ylab("Prevalence rate(per 100,000)")+
  labs(title="Ischemic Heart Disease")+
  theme_gray()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),
        legend.position=c(1,0),
        legend.justification=c(1,0),
        legend.background=element_blank(),
        legend.key.size=unit(0.1,"inches"))
p1
ggsave("Figure A.pdf",width=11.79,height=8,units="in",dpi=300)
#age base variation of Disease prevalence, by cohort
sort(unique(mydata$age))
p2<-mydata%>%
  filter(sex=="Both")|>
  ggplot(aes(x=age,y=rate,color=cohort_group))+
  geom_line(size=1)+
  geom_point(shape=2)+
  guides(color=guide_legend(title="cohort",ncol=2))+
  scale_color_manual(values=heat.colors(22))+
  scale_x_continuous(breaks=seq(from=15,to=98,by=5),
                     labels=unique(mydata$age_group))+
  xlab("")+
  ylab("Prevalence rate(per 100,000)")+
  theme_gray()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        legend.background=element_blank(),
        legend.key.size=unit(0.1,"inches"))
p2
ggsave("Figure B.pdf",width=11.79,height=8,units="in",dpi=300)

#cohort base variation of Disease prevalence, by period
p3 <- mydata%>%
  filter(sex=="Both")|>
  ggplot(aes(x=cohort_group,y=rate,group=period_group,color=period_group))+
  geom_line(size=1)+
  geom_point(shape=2)+
  guides(color=guide_legend(title="period",ncol=1))+
  scale_color_manual(values=heat.colors(6))+
  xlab("")+
  ylab("Prevalence rate(per 100,000)")+
  theme_gray()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),
        legend.position=c(1,1),
        legend.justification=c(1,1),
        legend.background=element_blank(),
        legend.key.size=unit(0.1,"inches"))
p3
ggsave("Figure C.pdf",width=11.79,height=8,units="in",dpi=300)
# 合并图片并写出 -----------------------------------------------------------------

library(patchwork)
p1/p2/p3+plot_annotation(tag_levels="A")
ggsave("Figure 8.pdf",width=11.79,height=20,units="in",dpi=300)

# 结果分析 --------------------------------------------------------------------

library(readxl)
results<-read_excel("APC 模型结果.xlsx",sheet=1,col_names=T)
results$group<-factor(results$group,
         levels=c("age","period","cohort"),
         labels=c("age","period","cohort"))
#生成一个x变量
x<-c(1:45)
results$x<-x
ggplot(data=results)+
  geom_ribbon(aes(x=x,ymin=lower,ymax=upper,fill=group),alpha=0.6)+
  geom_point(aes(x=x,y=coef,color=group))+
  geom_line(aes(x=x,y=coef,color=group),size=1)+
  scale_x_continuous(breaks=c(1:45),
                     labels=c(results$case))+
  scale_y_continuous(limits=c(-3,3))+
  scale_fill_discrete(name="Effect")+
  scale_color_discrete(name="Effect")+
  facet_grid(~group,scales="free_x",
             space="free_x")+
  xlab("")+
  ylab("Coefficients of APC model of Ischemic Heart Disease")+
  theme(axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0))
ggsave("Figure 9.pdf",width=11.79,height=8,units="in",dpi=300)

