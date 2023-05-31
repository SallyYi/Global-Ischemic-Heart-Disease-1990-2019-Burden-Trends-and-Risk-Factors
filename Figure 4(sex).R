rm(list = ls())
getwd()
library(tidyverse)
library(ggsci)
library(magrittr)
library(patchwork)

# 数据清洗 --------------------------------------------------------------------

df <- read_csv("ischemic_heart_disease.csv") 
df <- df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                               "cause_name","metric_name","year","val","upper","lower")]
colnames(df) <- c("measure","location","sex","age",
                  "cause","metric","year","val","upper","lower")
colnames(df)
unique(df$age)
df$val <- as.numeric(df$val)
df <- df|>
  select(measure,location,age,val,metric,year,sex) %>% 
  filter(location %in% c("Global","High SDI","High-middle SDI",
                         "Middle SDI","Low-middle SDI","Low SDI"),
         metric =="Rate",year=="2019") %>% 
  mutate(measure=case_when(measure=="DALYs (Disability-Adjusted Life Years)" ~ 
                             "DALYs",TRUE ~ as.character(measure)),
         val=as.numeric(val)) %>% 
  filter(age %in% c("15-19 years","20-24 years","25-29 years","30-34 years","35-39 years",
                    "40-44 years","45-49 years","50-54 years","55-59 years","60-64 years",
                    "65-69 years","70-74 years","75-79 years","80-84","85-89","90-94","95+ years")) %>% 
  mutate(across("age",str_replace,"1 to 4","<5")) %>% 
  mutate(across("age",str_replace,"95+ years","95+")) %>%
  mutate(across("age",str_replace," to ","-")) %>%
  mutate(across("age",str_replace,"80-84","80-84 years"))|>
  mutate(across("age",str_replace,"85-89","85-89 years"))|>
  mutate(across("age",str_replace,"90-94","90-94 years"))|>
  arrange(desc(measure))

# 定义因子 --------------------------------------------------------------------

df$age <- factor(df$age,levels= df$age %>% as.data.frame() %>% distinct() %>% pull())

df$location <- factor(df$location,
                      levels=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"))

df$measure <- factor(df$measure,levels=c("Incidence","Deaths","DALYs"))

# 整合数据 --------------------------------------------------------------------

dd1 <- df %>% filter(measure=="Incidence",sex !="Both")

df1 <- dd1 %>% filter(sex=="Male") %>% 
  left_join(.,dd1 %>% filter(sex=="Female"),by=c("location","age")) %>%
  dplyr::rename(Male="val.x",Female="val.y") %>% 
  mutate(`Male/Female`=Male/Female) %>% select(1,2,3,`Male/Female`) %>%
  mutate(across("age",str_replace,"95+ years","95+")) %>%
  mutate(across("age",str_replace," to ","-"))

df1$age <- factor(df1$age,levels=df1$age %>% as.data.frame() %>% distinct() %>% pull())
df1$location <- factor(df1$location,
                       levels=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"))

# 数据可视化 -------------------------------------------------------------------

ggplot(df1,aes(age,`Male/Female`,group=1,color="location"))+
  geom_point(size=1)+geom_line()+
  facet_wrap(.~location,nrow =3,scales = "free_y")+
  scale_color_nejm()+
  labs(x="age (years old)")+labs(y="Incidence Male/Female")+
  labs(x=NULL)+
  theme_bw()+
  theme(legend.position="non",strip.background.x = element_blank(),
        strip.text.x = element_text(size=10,color="black",face="bold"),
        #  axis.text.x=element_blank(),
        axis.text.x = element_text(color="black",angle = 45,vjust=1,hjust=1),
        axis.text.y=element_text(color="black"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),units=,"cm"),
        axis.title.y=element_text(color="black",margin = margin(r=5)),
        axis.title.x=element_text(color="black",margin = margin(t=5))
  )
ggsave("Figure 4.pdf",width=11.79,height=8,units="in",dpi=300)
