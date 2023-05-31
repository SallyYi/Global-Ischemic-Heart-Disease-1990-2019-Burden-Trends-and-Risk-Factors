setwd("/Users/jiangshali/Desktop/risk\ factor")
getwd()
library(dplyr)
library(ggplot2)
library(ggsci)

# Risk Factor for BMI -----------------------------------------------------

Risk <- read.csv('IHME-GBD_2019_DATA-f510aa30-1.csv',header = T)
Order <- read.csv('order.csv',header=F)
order$V1 <- rev(order$V1) ### ??ȡ?????겻ͬ??????????˳??
Risk_2019 <- subset(Risk, Risk$year==2019 & 
                      Risk$rei_name=='High body-mass index' &
                      Risk$metric_name=='Percent')
Risk_2019$val <- round(Risk_2019$val*100,1)
Risk_2019$val2 <- paste0(Risk_2019$val,'%')  
Risk_2019$location_name <- factor(Risk_2019$location_name, 
                             levels=order$V1, 
                             ordered=TRUE)  ###???????ꡪ?????????????ǵ?˳??��??ʾ
##??ͼ
unique(Risk_2019$rei_name)
p1 <- ggplot(Risk_2019,aes(location_name,weight = val, fill = measure_name))+
  geom_bar(color = 'black',width = .7,position = 'dodge',
           size = .3)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_nejm() +
  theme_classic()+
  coord_flip() + facet_grid(.~rei_name) + theme_light() +
  geom_text(aes(label=val2, y=val+1.5), ### ??????ֵҪ????ʵ????ͼ???????е???
            position=position_dodge(0.9), vjust=0,
            size = 2.5) 
p1

###
## All risk factors
## year=2019
Risk_2019 <- subset(Risk, Risk$year==2019 &
                      Risk$metric_name=='Percent')
Risk_2019$val <- round(Risk_2019$val*100,1)
Risk_2019$val2 <- paste0(Risk_2019$val,'%')
Risk_2019$location_name <- factor(Risk_2019$location_name, 
                             levels=Order$V1, 
                             ordered=TRUE)
unique(Risk_2019$rei_name)
Risk_2019$rei_name <- factor(Risk_2019$rei_name, 
                        levels= c('Dietary risks','High systolic blood pressure', 
                                  'High LDL cholesterol',
                                  'Tobacco',
                                  'Air pollution'), 
                        ordered=TRUE)  ## ???Լ???Ҫ??˳???ź?˳??
colnames(Risk_2019)
p1 <- ggplot(Risk_2019,aes(location_name,weight = val, fill = measure_name))+
  geom_bar(color = 'black',width = .7,position = 'dodge',
           size = .1)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_nejm() +
  theme_classic()+
  coord_flip() + facet_grid(.~rei_name) + theme_light() +
  geom_text(aes(label=val2, y=val+7.5), 
            position=position_dodge(0.9), vjust=0,
            size = 2.5) 
p1
p1 <- p1+xlab("GBD locations")+ylab("Percent of 2019")
ggsave("Figure 13.pdf",width=16,height=7.2,units="in",dpi=300,limitsize = FALSE)
