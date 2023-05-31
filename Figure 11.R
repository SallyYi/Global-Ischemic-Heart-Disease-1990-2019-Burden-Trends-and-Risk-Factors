setwd('/Users/jiangshali/Desktop/GBD')
getwd()
# 加载R包 --------------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(magrittr)
getwd()
setwd('/Users/jiangshali/Desktop/risk\ factor')
# 数据清洗 --------------------------------------------------------------------


risk1 <- read.csv("IHME-GBD_2019_DATA-a8413ac9-1.csv",header = T) 
colnames(risk1)
colnames(risk1) <- c("measure_id","measure","location_id","location","sex_id","sex","age_id","age",
                     "cause_id","cause","rei_id","rei","metric_id","metric","year","val","upper","lower")
risk1 <- risk1%>% filter(age=="All ages",cause=="Ischemic heart disease",metric=="Number",year=="1990") %>%   
  arrange(desc(val)) %>% 
  select(rei) %>% 
  rownames_to_column(var="id") %>% 
  unite(.,col="rei_2",id,rei,sep=" ",remove = F,na.rm = F) %>% 
  mutate(g=1,grp="risks 1990")

risk2 <- read.csv("IHME-GBD_2019_DATA-a8413ac9-1.csv",header = T) 
colnames(risk2)
colnames(risk2) <- c("measure_id","measure","location_id","location","sex_id","sex","age_id","age",
                     "cause_id","cause","rei_id","rei","metric_id","metric","year","val","upper","lower")
risk2 <- risk2%>% 
  filter(age=="All ages",cause=="Ischemic heart disease",metric=="Number",year=="2019") %>% 
  arrange(desc(val)) %>% 
  select(rei) %>% 
  rownames_to_column(var="id") %>% 
  unite(.,col="rei_2",id,rei,sep=" ",remove = F,na.rm = F) %>% 
  mutate(g=2,grp="risks 2019") 

# 合并表格 --------------------------------------------------------------------

df <- rbind(risk1,risk2) %>% group_by(grp) %>%
  ungroup() %>% 
  mutate(id=rev(as.numeric(id)))

colnames(t10)
t10 <- read.csv("IHME-GBD_2019_DATA-a8413ac9-1.csv",header=T) 
colnames(t10) <- c("measure_id","measure","location_id","location","sex_id","sex","age_id","age",
                     "cause_id","cause","rei_id","rei","metric_id","metric","year","val","upper","lower")
t10 <-t10%>% 
  filter(age=="All ages",cause=="Ischemic heart disease",metric=="Number",year=="2019") %>% 
  arrange(desc(val)) %>% select(rei,year,val) 

t11 <- read.csv("IHME-GBD_2019_DATA-a8413ac9-1.csv",header=T) 
colnames(t11) <- c("measure_id","measure","location_id","location","sex_id","sex","age_id","age",
                   "cause_id","cause","rei_id","rei","metric_id","metric","year","val","upper","lower")
t11 <-t11%>% 
  filter(age=="All ages",cause=="Ischemic heart disease",metric=="Number",year=="1990") %>% 
  arrange(desc(val)) %>% select(rei,year,val) 

t1 <- left_join(t10,t11,by="rei") %>% 
  mutate(per=(val.y-val.x)/val.x) %>% select(rei,per) %>% 
  rownames_to_column(var="id") %>% 
  mutate(g=2,grp="risks 2019",id=rev(as.numeric(id)))

t20 <- read.csv("IHME-GBD_2019_DATA-a8413ac9-1.csv",header=T) 
colnames(t20) <- c("measure_id","measure","location_id","location","sex_id","sex","age_id","age",
                   "cause_id","cause","rei_id","rei","metric_id","metric","year","val","upper","lower")
t20 <-t20%>% 
  filter(age=="All ages",cause=="Ischemic heart disease",metric=="Percent",year=="2019") %>% 
  arrange(desc(val)) %>% select(rei,year,val) 

t21 <- read.csv("IHME-GBD_2019_DATA-a8413ac9-1.csv",header=T) 
colnames(t21) <- c("measure_id","measure","location_id","location","sex_id","sex","age_id","age",
                   "cause_id","cause","rei_id","rei","metric_id","metric","year","val","upper","lower")
t21 <-t21%>% 
  filter(age=="All ages",cause=="Ischemic heart disease",metric=="Percent",year=="1990") %>% 
  arrange(desc(val)) %>% select(rei,year,val) 

t2 <- left_join(t20,t21,by="rei") %>% 
  mutate(per=(val.y-val.x)/val.x) %>% select(rei,per) %>% 
  rownames_to_column(var="id") %>% 
  mutate(g=2,grp="risks 2019",id=rev(as.numeric(id)))

selected <- df %>% select(rei, g) %>% 
  count(rei) %>% filter(n==2) %>% pull(rei)
# 数据可视化 -------------------------------------------------------------------

df %>% 
  ggplot(aes(x=g, y=id)) +
  geom_segment(data=df %>% filter(g==1),aes(x=g, xend=g-2,y=id,yend=id),size=6,
               color="#F5A300",alpha=0.6)+
  geom_text(data=df %>% filter(g==1),
            aes(x=g-1,xend=g,label=rei_2,y=id,yend=id),size=3.5,color="black")+
  geom_segment(data=df %>% filter(g==2),aes(x=g,xend=g+2,y=id,yend=id),size=6,
               color="#5686C3",alpha=0.5)+
  geom_text(data=df %>% filter(g==2),
            aes(x=g+1,xend=g,label=rei_2,y=id,yend=id),size=3.5,color="black")+
  geom_line(data=df %>% filter(rei %in% selected),aes(group=rei),color="#F5A300")+
  geom_segment(data=df,aes(x=4.01, xend=4.5,y=id,yend=id),size=6,color="#973CB6",alpha=0.5)+
  geom_text(data=t1,aes(x=4.2,xend=5,label=round(per,digits=2),
                        y=id,yend=id),size=3.5,color="black")+
  geom_segment(data=df,aes(x=4.51, xend=5.1,y=id,yend=id),size=6,color="#75C500",alpha=0.5)+
  geom_text(data=t2,aes(x=4.75,xend=4.9,label=round(per,digits=2),
                        y=id,yend=id),size=3.5,color="black")+
  scale_y_continuous(limits=c(0,19)) +
  annotate(geom="text",y=18,x=0, label="risks 1990",size=4.3, fontface="bold") +
  annotate(geom="text",y=18,x=3, label="risks 2019",size=4.3, fontface="bold") +
  annotate(geom="text",y=17,x=4.2, label="Mean\npercentage\nchange\nin number\nof DALYs,\n1990-2019 ",
           size=2.5,fontface="bold") +
  annotate(geom="text",y=17,x=4.8, label="Mean\npercentage\nchange\nin all-age\nof DALYs rate,\n1990-2019 ",
           size=2.5,fontface="bold") +
  theme_void()+
  theme(legend.position = "none")
ggsave("Figure 11.pdf",width=12,height=5.2,units="in",dpi=300,limitsize = FALSE)
