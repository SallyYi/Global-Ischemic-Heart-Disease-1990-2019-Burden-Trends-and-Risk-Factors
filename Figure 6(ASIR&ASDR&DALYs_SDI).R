setwd("/Users/jiangshali/Desktop/GBD")
getwd()
library(tidyverse)

list.files(path = "/Users/jiangshali/Desktop/GBD",
           pattern = "*.csv", full.names=TRUE) %>% 
  lapply(read_csv) %>% bind_rows() %>% write_csv(.,file="ischemic_heart_disease.csv",quote="none")

library(tidyverse)
library(ggh4x)
library(ggsci)
library(magrittr)
library(grid)

# 提取数据 --------------------------------------------------------------------
df <- read_csv("ischemic_heart_disease.csv")
colnames(df)
newdf <- df[,colnames(df)%in%c("measure_name","location_name","sex_name","age_name",
                               "cause_name","metric_name","year","val","upper","lower")]
colnames(newdf[colnames(newdf)%in%c('measure_name','location_name','sex_name','age_name',
                               'cause_name','metric_name','year','val','upper','lower')] <- 
           c('measure','location','sex','age','cause','metric','year','val','upper','lower')
colnames(newdf)         
unique(newdf$measure_name)         

# 数据筛选 --------------------------------------------------------------------

df1 <- newdf %>% select(measure_name,year,val,location_name,age_name,metric_name,sex_name) %>% 
  filter(location_name %in% c("Global","High SDI",
                         "High-middle SDI","Middle SDI",
                         "Low-middle SDI","Low SDI"),
         age_name == 'Age-standardized',metric_name=="Rate",measure_name !="Prevalence") %>%
  select(measure_name,val,location_name,year,sex_name) %>% 
  mutate(measure_name=case_when(measure_name=="DALYs (Disability-Adjusted Life Years)" ~ 
                             "DALYs per 100,000",
                           TRUE ~ as.character(measure_name))) %>% 
  mutate(across("measure_name",str_replace,"Deaths","ASDR per 100,000")) %>% 
  mutate(across("measure_name",str_replace,"Incidence","ASIR per 100,000")) %>% 
  mutate(val=as.numeric(val),year=as.numeric(year),sex_name=as.factor(sex_name)) %>% 
  unite(.,col="measure_name",sex_name,measure_name,sep="-",remove = T,na.rm = F)

# 定义因子 --------------------------------------------------------------------

df1$measure_name <- factor(df1$measure_name,levels=c("Both-ASIR per 100,000","Both-ASDR per 100,000",
                                           "Both-DALYs per 100,000",
                                           "Female-ASIR per 100,000","Female-ASDR per 100,000",
                                           "Female-DALYs per 100,000",
                                           "Male-ASIR per 100,000","Male-ASDR per 100,000",
                                           "Male-DALYs per 100,000"))

# 定义分面背景 ------------------------------------------------------------------

ridiculous_strips <- strip_themed(
  text_x = elem_list_text(colour=c("#9C8D58","#EDB749","#3CB2EC"),
                          face=c("bold","bold","bold"),size=c(10,10,10)),
  background_x = elem_list_rect(fill = c("#EEECE1","#FFF2E7","#E8F2FC")))

# 定义注释函数 ------------------------------------------------------------------

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

# 数据可视化 -------------------------------------------------------------------

df1 %>% ggplot(aes(year,val,color=location_name,fill=location_name))+
  geom_point(size=2,pch=21,color="white")+geom_line()+
  facet_wrap2(vars(measure_name),scales = "free_y",axes = "y",remove_labels = "x",strip = ridiculous_strips)+
  guides(color=guide_legend(title="location_name",nrow=1))+
  scale_color_jco()+
  scale_fill_jco()+
  coord_cartesian(clip="off")+
  theme_bw()+
  theme(panel.spacing.x = unit(0.1,"cm"),
        panel.spacing.y = unit(0.15,"cm"),
        axis.title = element_blank(),
        strip.background = element_rect(fill="grey80"),
        strip.text.x = element_text(size=9,color="black"),
        axis.text = element_text(color="black"),
        plot.margin=unit(c(0.2,0.5,0.2,0.2),units=,"cm"),
        legend.text=element_text(color="black",size=9,face="bold"),
        legend.key=element_blank(),  
        legend.title = element_blank(),
        legend.position = "top",
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank())+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="grey90",col="black")),
                     data = df1 %>% filter(measure_name=="Both-ASDR per 100,000"),
                     xmin =1954.5,xmax =2055.5,ymin=18.5,ymax=20)

# 结果保存 --------------------------------------------------------------------

ggsave("Figure 6.pdf",width=11.79,height=10.42,units="in",dpi=300)
