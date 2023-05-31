install.packages("MetBrewer")

library(tidyverse)
library(ggh4x)
library(MetBrewer)
library(magrittr)

df <- read_csv("ischemic_heart_disease.csv")
df <- df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                               "cause_name","metric_name","year","val","upper","lower")]
colnames(df) <- c("measure","location","sex","age",
                  "cause","metric","year","val","upper","lower")
colnames(df)
unique(df$age)
# 数据清洗 --------------------------------------------------------------------

df %<>% filter(metric == 'Number',measure %in% c('Incidence',"Deaths"),
               age %in% c("20-24 years","25-29 years","30-34 years","35-39 years","40-44 years",
                          "45-49 years","50-54 years","55-59 years","60-64 years","65-69 years",
                          "70-74 years","75-79 years","All ages","80-84","85-89","90-94","95+ years"),
               location %in% c("Global","High SDI","High-middle SDI","Middle SDI",
                               "Low-middle SDI","Low SDI","Andean Latin America",
                               "Australasia","Caribbean","Central Asia",
                               "Central Europe","Central Latin America",
                               "Central Sub-Saharan Africa","East Asia",
                               "Eastern Europe","Eastern Sub-Saharan Africa",
                               "High-income Asia Pacific","High-income North America",
                               "North Africa and Middle East","Oceania","South Asia",
                               "Southeast Asia","Southern Latin America",
                               "Southern Sub-Saharan Africa","Tropical Latin America",
                               "Western Europe","Western Sub-Saharan Africa"),
               year %in% c("1990","2019")) %>%
  mutate(across("age",str_replace,"20-24 years","20 to 39")) %>% 
  mutate(across("age",str_replace,"25-29 years","20 to 39")) %>% 
  mutate(across("age",str_replace,"30-34 years","20 to 39")) %>% 
  mutate(across("age",str_replace,"35-39 years","20 to 39")) %>% 
  mutate(across("age",str_replace,"40-44 years","40 to 59")) %>% 
  mutate(across("age",str_replace,"45-49 years","40 to 59")) %>% 
  mutate(across("age",str_replace,"50-54 years","40 to 59")) %>% 
  mutate(across("age",str_replace,"55-59 years","40 to 59")) %>% 
  mutate(across("age",str_replace,"60-64 years","60 to 79")) %>% 
  mutate(across("age",str_replace,"65-69 years","60 to 79")) %>% 
  mutate(across("age",str_replace,"70-74 years","60 to 79")) %>% 
  mutate(across("age",str_replace,"75-79 years","60 to 79")) %>%
  mutate(across("age",str_replace,"80-84","80 to 94")) %>% 
  mutate(across("age",str_replace,"85-89","80 to 94")) %>% 
  mutate(across("age",str_replace,"90-94","80 to 94")) %>% 
  mutate(across("age",str_replace,"95+ years","95+")) %>% 
  mutate(year=as.character(year)) %>% 
  mutate(val=as.numeric(val)) %>% 
  group_by(measure,location,age,year) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm=TRUE))) %>% 
  arrange(match(location,c("Global","High SDI",
                           "High-middle SDI","Middle SDI",
                           "Low-middle SDI","Low SDI",
                           "Andean Latin America","Australasia",
                           "Caribbean","Central Asia","Central Europe",
                           "Central Latin America","Central Sub-Saharan Africa",
                           "East Asia","Eastern Europe","Eastern Sub-Saharan Africa",
                           "High-income Asia Pacific","High-income North America",
                           "North Africa and Middle East","Oceania","South Asia",
                           "Southeast Asia","Southern Latin America",
                           "Southern Sub-Saharan Africa","Tropical Latin America",
                           "Western Europe","Western Sub-Saharan Africa"))) %>% 
  ungroup()
  
colnames(df)
# 计算每一段发病率死亡率 -------------------------------------------------------------

f4 <- df %>% arrange(year,measure) %>% filter(age !="All ages")
newdf <- df %>% arrange(year,measure) %>% 
  filter(age=="All ages") %>% 
  slice(rep(1:n(),each=5))
                                    
                                    
f3 <- f4%>% 
  left_join(.,newdf,
            by=c("location","measure","year")) %>% 
  distinct() %>% mutate(per=val.x/val.y*100)

# 数据可视化 -------------------------------------------------------------------

p <- f3 %>% select(-age.y,-val.y,-val.x) %>%
  mutate(per=round(per,digits = 2)) %>% 
  mutate(location=factor(location,
                         levels=rev(c("Global","High SDI",
                                      "High-middle SDI","Middle SDI",
                                      "Low-middle SDI","Low SDI",
                                      "Andean Latin America","Australasia",
                                      "Caribbean","Central Asia","Central Europe",
                                      "Central Latin America","Central Sub-Saharan Africa",
                                      "East Asia","Eastern Europe","Eastern Sub-Saharan Africa",
                                      "High-income Asia Pacific","High-income North America",
                                      "North Africa and Middle East","Oceania","South Asia",
                                      "Southeast Asia","Southern Latin America",
                                      "Southern Sub-Saharan Africa","Tropical Latin America",
                                      "Western Europe","Western Sub-Saharan Africa"))
  )) %>% 
  ggplot(aes(location,per,fill=age.x))+
  geom_col()+
  coord_flip()+
  facet_nested(.~measure+year,drop=T,scale="free",space="free",switch="y")+
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))+
  labs(x=NULL,y=NULL)+
  theme(panel.spacing.x = unit(0.001,"cm"),
        strip.background = element_rect(fill="grey90"),
        strip.text.x =  element_text(size=9,color="black"),
        axis.text.y=element_text(color="black",colour = 12),
        axis.ticks=element_blank(),
        plot.margin=unit(c(0.2,0.5,0.2,0.2),units=,"cm"),
        legend.text=element_text(color="black",size=9),
        legend.key=element_blank(),  
        legend.spacing.x=unit(0.2,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank(),
        legend.position = "top")+
  guides(fill=guide_legend(title="age"))+
  scale_fill_manual(values=met.brewer("Veronese",n=6, type="discrete"))
p
ggsave("Figure 3.pdf",width=11.79,height=8,units="in",dpi=300)
