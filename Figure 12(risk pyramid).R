rm(list=ls())
setwd("/Users/jiangshali/Desktop/risk\ factor")
getwd()

# 加载包 ---------------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(magrittr)
library(ggprism)
library(MetBrewer)

# 数据清洗 --------------------------------------------------------------------

df <- read_csv("IHME-GBD_2019_DATA-5bd06db5-1.csv")
colnames(df)
df <- df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                            "cause_name","rei_name","metric_name","year","val","upper","lower")]
colnames(df) <- c("measure","location","sex","age",
                  "cause","rei","metric","year","val","upper","lower")
colnames(df)
unique(df$age)
df <- df|>select(-upper,-lower,-1,-2,-cause) %>% 
  filter(sex !="Both",metric=="Number",age !="<5 years") %>% 
  mutate(val=val/1000000)
df <- df|>
  mutate(across("age",str_replace,"80-84","80-84 years")) %>% 
  mutate(across("age",str_replace,"85-89","85-89 years")) %>% 
  mutate(across("age",str_replace,"90-94","90-94 years"))
# 数据可视化 -------------------------------------------------------------------

p <- df %>% 
  ggplot(aes(y=age,x = ifelse(sex=="Female",-val,val),fill =rei)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values=met.brewer("Hiroshige",13))+
  scale_x_continuous(guide = "prism_minor",expand = c(0,0),limits=c(-70,70),
                     breaks=seq(-70,70,10))+
  geom_vline(xintercept = 0,linetype = 2)+ # 添加垂直线
  labs(x="DALYs (millions)",y=NULL)+
  theme(axis.text.y=element_text(color="black",size=8,margin=margin(r=1)),
        axis.text.x=element_text(color="black",size=9,margin=margin(t=12)),
        axis.title.x = element_text(size=11,margin=margin(t=8),color="black",face="bold"),
        plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"), 
        panel.background = element_blank(),   # 移除灰色背景框
        prism.ticks.length.y = unit(3, "pt"),
        prism.ticks.length.x = unit(-5, "pt"),
        axis.line = element_line(color="black"),
        axis.ticks.length.x = unit(-0.3, "cm"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=8,color="black"),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.4,"cm"),
        legend.key.height=unit(0.4,"cm"),
        legend.position =c(0.001,1.02), # 定义图例位置
        legend.justification = c(0,1)
  )+
  annotate(geom="text",y=17,x=15,label="Males",size=4,fontface="bold")+
  annotate(geom="text",y=17,x=-15,label="Females",size=4,fontface="bold")

p
ggsave("Figure 12.pdf",width=11.79,height=8,units="in",dpi=300)

