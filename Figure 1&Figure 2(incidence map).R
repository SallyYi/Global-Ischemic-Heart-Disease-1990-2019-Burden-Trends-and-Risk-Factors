setwd("/Users/jiangshali/Desktop/GBD")
getwd()

library(sf)
library(patchwork)
library(ggplot2)
library(tidyverse)
GBD <- read_csv("ischemic_heart_disease.csv")
GBD <- GBD[,colnames(GBD)%in%c("measure_name","location_name","sex_name","age_name",
                               "cause_name","metric_name","year","val","upper","lower")]
colnames(GBD) <- c("measure","location","sex","age",
                   "cause","metric","year","val","upper","lower")
colnames(GBD)
location <- read_csv("location.csv")
GBD <- left_join(GBD,location,by="location")
GBD<-na.omit(GBD)
# shp数据的读取与配置
map <- st_read("世界国家.shp") #
map <- st_set_crs(map,4326)
## 绘制数据读取
unique(GBD$age)
df <- GBD|>
  filter(measure=="Incidence") %>% 
  filter(year==2019)  %>% 
  filter(metric=="Number")  %>% 
  filter(age=="All ages")  %>% 
  filter(sex=="Both")
str(df)
df$val <- as.numeric(df$val)
main_map_data <- left_join(map,df,by=c("NAME"="location3")) 
  
str(main_map_data)
head(main_map_data)
p <- main_map_data  %>% 
  ggplot()+
  geom_sf(aes(group=NAME,fill=val),color='black',size = 0.5) +
  theme_void()+
  scale_fill_distiller(palette="Spectral",# 色盘
                       name="cases") +
  labs(x="",y="",title="")+ 
  theme(legend.position = c(0.1,0.2),
        legend.title = element_text(color="black", 
                                    size = 10, 
                                    #family = "A",
                                    #face = "bold"
        ),
        plot.title = element_text(color="black", 
                                  size = 14, 
                                  #family = "A",
                                  #face = "bold"
        ),
        legend.text = element_text(color="black", 
                                   size = 10, 
                                   #family = "A",
                                   #face = "bold"
        ),
        panel.grid=element_blank(),
        #legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
  )
p
ggsave("Figure 2.pdf",width=30,height=18,units="in",dpi=900)

worldData <- map_data('world')
small_map_data <- df %>% 
  filter(measure=="Incidence") %>% 
  filter(year==2019)  %>% 
  filter(sex=="Both")

small_map_data$location[small_map_data$location == 'United States of America'] = 'USA'
small_map_data$location[small_map_data$location == 'Russian Federation'] = 'Russia'
small_map_data$location[small_map_data$location == 'United Kingdom'] = 'UK'
small_map_data$location[small_map_data$location == 'Congo'] = 'Republic of Congo'
small_map_data$location[small_map_data$location == "Iran (Islamic Republic of)"] = 'Iran'
small_map_data$location[small_map_data$location == "Democratic People's Republic of Korea"] = 'North Korea'
small_map_data$location[small_map_data$location == "Taiwan (Province of China)"] = 'Taiwan'
small_map_data$location[small_map_data$location == "Republic of Korea"] = 'South Korea'
small_map_data$location[small_map_data$location == "United Republic of Tanzania"] = 'Tanzania'
small_map_data$location[small_map_data$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
small_map_data$location[small_map_data$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
small_map_data$location[small_map_data$location == "Czechia"] = 'Czech Republic'
small_map_data$location[small_map_data$location == "Republic of Moldova"] = 'Moldova'
small_map_data$location[small_map_data$location == "Viet Nam"] = 'Vietnam'
small_map_data$location[small_map_data$location == "Lao People's Democratic Republic"] = 'Laos'
small_map_data$location[small_map_data$location == "Syrian Arab Republic"] = 'Syria'
small_map_data$location[small_map_data$location == "North Macedonia"] = 'Macedonia'
small_map_data$location[small_map_data$location == "Micronesia (Federated States of)"] = 'Micronesia'
small_map_data$location[small_map_data$location == "Macedonia"] = 'North Macedonia'
small_map_data$location[small_map_data$location == "Trinidad and Tobago"] = 'Trinidad'
a <- small_map_data[small_map_data$location == "Trinidad",]
a$location <- 'Tobago'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Cabo Verde"] = 'Cape Verde'
small_map_data$location[small_map_data$location == "United States Virgin Islands"] = 'Virgin Islands'
small_map_data$location[small_map_data$location == "Antigua and Barbuda"] = 'Antigu'
a <- small_map_data[small_map_data$location == "Antigu",]
a$location <- 'Barbuda'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- small_map_data[small_map_data$location == "Saint Kitts",]
a$location <- 'Nevis'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Côte d'Ivoire"] = 'Ivory Coast'
small_map_data$location[small_map_data$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- small_map_data[small_map_data$location == "Saint Vincent",]
a$location <- 'Grenadines'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Eswatini"] = 'Swaziland'
small_map_data$location[small_map_data$location == "Brunei Darussalam"] = 'Brunei'

small_map_data <- full_join(worldData,small_map_data,by = c('region'='location')) %>%   
  filter(val != "NA")
dim(small_map_data)
head(small_map_data)

fig <- small_map_data %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat,group = group,fill=val),
               colour="black",size=0.5) +
  theme_bw()+
  scale_fill_distiller(palette="Spectral",# 色盘
                       name="ASR") + 
  theme(legend.position = 'none',
        legend.title = element_blank(),
        plot.title = element_text(color="black", 
                                  size = 10, 
                                  #family = "A",
                                  #face = "bold"
        ),
        legend.text = element_text(color="black", 
                                   size = 12, 
                                   #family = "A",
                                   #face = "bold"
        ),
        panel.grid=element_blank(),
        panel.border = element_rect(color='black',
                                    fill=NA,
                                    linewidth = 0.5),
        #legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
  )

p2 <- fig+ labs(x=" ",y="",title="Caribbean and central America")+
  coord_cartesian(xlim = c(-92,-60),ylim = c(5,27))

p3 <- fig+ labs(x=" ",y="",title="Persian Gulf")+
  coord_cartesian(xlim = c(45,55),ylim = c(19,31))

p4 <- fig+ labs(x=" ",y="",title="Balkan Peninsula")+
  coord_cartesian(xlim =  c(12,32),ylim = c(35,53))

p5 <- fig+ labs(x=" ",y="",title="Sotheast Asia")+
  coord_cartesian(xlim =  c(98,123),ylim = c(-10,8))

p6 <- fig+ labs(x=" ",y="",title="West Africa") +
  coord_cartesian(xlim =  c(-17,-7),ylim = c(7,20))

p7 <- fig+ labs(x=" ",y="",title="Eastern \nMediterranean")+
  coord_cartesian(xlim =  c(32,37),ylim = c(29,35))

p8 <- fig+ labs(x=" ",y="",title="Northern Europe") +
  coord_cartesian(xlim =  c(5,25),ylim = c(48,60))

A= (p6|p7)/p8 

plot<-  p +
  (p2+p3+p4+p5+A+plot_layout(ncol = 5,widths=c(1.5,1,1.1,1.2,1)))+ 
  plot_layout(ncol = 1,heights = c(9, 3)) 
plot
ggsave("Figure 1.pdf",width=20,height=12,units="in",dpi=900)
