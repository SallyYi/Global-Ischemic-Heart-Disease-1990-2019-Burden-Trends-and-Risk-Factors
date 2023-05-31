install.packages("magrittr")
library(tidyverse)
getwd()

list.files(path = "/Users/jiangshali/Desktop/GBD",
           pattern = "*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% bind_rows() %>% write_csv(.,file="ischemic_heart_disease_cancer.csv",quote="none")

df <- read_csv("ischemic_heart_disease.csv")
colnames(df)
newdf <- df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                               "cause_name","metric_name","year","val","upper","lower")]
unique(newdf$age_name)
colnames(newdf) <- c("measure","location","sex","age",
                     "cause","metric","year","val","upper","lower")
colnames(newdf)
unique(newdf$location)
unique(newdf$measure)
unique(newdf$age)

# 加载R包 --------------------------------------------------------------------

library(tidyverse)
library(ggsci)
library(ggh4x)

# 数据清洗 --------------------------------------------------------------------
newdf <- newdf %>%
  filter(age %in% c("15-19 years","20-24 years","25-29 years","30-34 years",    
                    "35-39 years","40-44 years","45-49 years","50-54 years",     
                    "55-59 years","60-64 years","65-69 years","70-74 years",     
                    "75-79 years","80-84","85-89","90-94","95+ years"),
         year %in% c("2009","2019"),metric=="Rate") %>% 
  filter(location %in% c("Global","High SDI","High-middle SDI",
                         "Middle SDI","Low-middle SDI","Low SDI")) %>%
  select(measure,location,sex,year,age,val) %>% 
  mutate(year=as.character(year)) %>% 
  mutate(across("age",str_replace,"95+ years","95+")) %>% 
  mutate(across("age",str_replace,"80-84","80-84 years")) %>% 
  mutate(across("age",str_replace,"85-89","85-89 years")) %>% 
  mutate(across("age",str_replace,"90-94","90-94 years")) %>% 
  mutate(across("measure",str_replace,"Incidence","Incidence rate (per 100k)")) %>%
  mutate(across("measure",str_replace,"Deaths","Deaths rate (per 100k)")) %>%
  mutate(measure=case_when(measure=="DALYs" ~ 
                             "DALYs rate (per 100k)",
                           TRUE ~ as.character(measure))) %>% 
  arrange(measure)

# 定义因子 --------------------------------------------------------------------


newdf$age <- factor(newdf$age,levels=c(newdf$age %>% as.data.frame() %>% distinct() %>% filter(.!="5 to 9") %>% 
                                   dplyr::rename(age=".") %>% 
                                   add_row(age="5 to 9",.before = 2) %>% pull()
))


newdf$year <- factor(newdf$year,levels=c("2009","2019"))
as.numeric(newdf$age)
# 数据可视化 -------------------------------------------------------------------

newdf %>% arrange(year) %>% filter(measure=="Deaths rate (per 100k)",sex=="Both") %>% 
  unite(.,col="location",location,measure,sep=" ",
        remove = T,na.rm = F) %>% 
  ggplot()+
  geom_line(aes(val,age,group=age),size=1.0,color="grey80")+
  geom_point(aes(val,age,color=year),size=2)+
  facet_wrap2(vars(location), nrow = 2, ncol = 3, trim_blank = FALSE)+
  xlab(NULL)+ylab(NULL)+
  scale_color_manual(values = c("#009688","#762a83"))+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_line(color = "#4a4e4d"),
    axis.text=element_text(color="black",face="bold"),
    strip.text = element_text(color="black",face="bold"),
    panel.background = element_rect(fill = "white",color = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(0,"lines"),
    plot.title = element_blank(),
    legend.text = element_text(color="black",face="bold"),
    legend.title = element_blank(),
    legend.key=element_blank(),  
    legend.spacing.x=unit(0.1,'cm'), 
    legend.key.width=unit(0.4,'cm'), 
    legend.key.height=unit(0.4,'cm'), 
    legend.background=element_blank(), 
    legend.position = c(0.08,1), legend.justification = c(1,1))

ggsave("Figure 7.pdf",width=10,height=7.2,units="in",dpi=300,limitsize = FALSE)
