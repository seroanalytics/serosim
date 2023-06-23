## Load necessary packages
library(ggplot2)
library(tidyverse)
library(cowplot)

## Import run time data set 
# run_time_path <- system.file("run_time", "serosim_run_time_updated.csv", package = "serosim")
# df <- read.csv(file = run_time_path, header = TRUE)

library(readr)
serosim_run_time_updated <- read_csv("inst/scripts/run_time/serosim_run_time_updated.csv")
df<-serosim_run_time_updated


## If run times are over 120 seconds or are NA then set to 120 seconds
## Times that are NA are because the initial runs were over 120 seconds and so the 100 simulations were not done
df$min<-ifelse(df$min>120 | is.na(df$min),120,df$min)
df$max<-ifelse(df$max>120 | is.na(df$max),120,df$max)
df$median<-ifelse(df$median>120| is.na(df$median),120,df$median)
df$mean<-ifelse(df$mean>120| is.na(df$mean),120,df$mean)
df$first_q<-ifelse(df$first_q>120| is.na(df$first_q),120,df$first_q)
df$third_q<-ifelse(df$third_q>120| is.na(df$third_q),120,df$third_q)



## Separate run time by case study 
df_readme <-  df %>% filter(df$example=="quick_start")
df_cs1<-df %>% filter(df$example=="CS1")
df_cs2 <-df %>% filter(df$example=="CS2")
df_cs3 <-df %>% filter(df$example=="CS3")

theme_set(theme_bw()) 

## Create each individual plot 
p_r<- ggplot(df_readme, aes(x=as.factor(individuals), y=(mean/60),  colour=as.factor(times)))  
p_r<- p_r + geom_boxplot(aes(x=as.factor(individuals), ymin=(min/60), ymax=(max/60), lower= (first_q/60), middle=(median/60), upper=(third_q/60), group=interaction(as.factor(individuals), as.factor(times))), stat = "identity", width=0.5) +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="README example",
       subtitle = "2 exposure events, 1 biomarker",
       key ="test") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(color = guide_legend(title = "Number of time steps")) + scale_color_viridis_d(option="D", end=0.65)+
  scale_y_continuous(labels = c(0,0.5,1,1.5,">2"))




p_1<- ggplot(df_cs1, aes(x=as.factor(individuals), y=(mean/60),  colour=as.factor(times)))  
p_1<- p_1 +geom_boxplot(aes(x=as.factor(individuals), ymin=(min/60), ymax=(max/60), lower= (first_q/60), middle=(median/60), upper=(third_q/60), group=interaction(as.factor(individuals), as.factor(times))), stat = "identity", width=0.5) +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 1",
       subtitle = "2 exposure events, 1 biomarker",
       key ="test") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(color = guide_legend(title = "Number of time steps")) + scale_color_viridis_d(option="D", end=0.65)+
  scale_y_continuous(labels = c(0,0.5,1,1.5,">2"))


p_2<- ggplot(df_cs2, aes(x=as.factor(individuals), y=(mean/60),  colour=as.factor(times)))  
p_2<- p_2 +geom_boxplot(aes(x=as.factor(individuals), ymin=(min/60), ymax=(max/60), lower= (first_q/60), middle=(median/60), upper=(third_q/60), group=interaction(as.factor(individuals), as.factor(times))), stat = "identity", width=0.5) +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 2",
       subtitle = "3 exposure events, 2 biomarkers",
       key ="test") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(color = guide_legend(title = "Number of time steps")) + scale_color_viridis_d(option="D", end=0.65)+
  scale_y_continuous(labels = c(0,0.5,1,1.5,">2"))


## Create additional plot for case study 3
p_3<- ggplot(df_cs3, aes(x=as.factor(individuals), y=(mean/60),  colour=as.factor(times)))
p_3<- p_3 +geom_boxplot(aes(x=as.factor(individuals), ymin=(min/60), ymax=(max/60), lower= (first_q/60), middle=(median/60), upper=(third_q/60), group=interaction(as.factor(individuals), as.factor(times))), stat = "identity", width=0.5) +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 3",
       subtitle = "10 exposure events, 10 biomarkers",
       key ="test") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(color = guide_legend(title = "Number of time steps")) + scale_color_viridis_d(option="D", end=0.65)+
  scale_y_continuous(labels = c(0,0.5,1,1.5,">2"))


## Combine all 4 plots
plot_grid(p_r,p_1,p_2,p_3, nrow=2, ncol=2, align = "hv", scale=c(.98,.98, .98, .98))
## Export 10 x 10 



## Create run time fgiures for the supplement
df<-serosim_run_time_updated
df_readme <-  df %>% filter(df$example=="quick_start")
df_cs1<-df %>% filter(df$example=="CS1")
df_cs2 <-df %>% filter(df$example=="CS2")
df_cs3 <-df %>% filter(df$example=="CS3")


#### X axis: Number of time steps 

## README 
data<-df_readme

p_r<-ggplot(data %>% filter(individuals==100), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1) +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "README: \n 100 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_1<-ggplot(data %>% filter(individuals==500), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "README: \n 500 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_2<-ggplot(data %>% filter(individuals==1000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "README: \n 1000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_3<-ggplot(data %>% filter(individuals==5000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "README: \n 5000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))
full1<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))


## Case study 1 
data<-df_cs1

p_r<-ggplot(data %>% filter(individuals==100), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1) +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 1:\n 100 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_1<-ggplot(data %>% filter(individuals==500), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 1:\n 500 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_2<-ggplot(data %>% filter(individuals==1000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 1: \n 1000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_3<-ggplot(data %>% filter(individuals==5000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 1: \n 5000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))
full2<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))


##Case study 2 
data<-df_cs2

p_r<-ggplot(data %>% filter(individuals==100), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1) +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 2: \n 100 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_1<-ggplot(data %>% filter(individuals==500), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 2: \n 500 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_2<-ggplot(data %>% filter(individuals==1000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 2: \n 1000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_3<-ggplot(data %>% filter(individuals==5000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 2: \n 5000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))
full3<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))



## Case study 3 
data<-df_cs3

p_r<-ggplot(data %>% filter(individuals==100), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1) +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 3: \n 100 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_1<-ggplot(data %>% filter(individuals==500), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 3: \n 500 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_2<-ggplot(data %>% filter(individuals==1000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 3: \n 1000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))

p_3<-ggplot(data %>% filter(individuals==5000), aes(x=times, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of time steps",
       title = "Case study 3: \n 5000 individuals") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))
full4<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))



plot_grid(full1,full2,full3,full4, nrow=4, ncol=1, align = "hv", scale=c(.98,.98, .98, .98))
## Export 10x10













#### X axis: Number of individuals

data<-df_readme

p_r<-ggplot(data %>% filter(times==10), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1)+
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="README: \n 10 time steps ") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_1<-ggplot(data %>% filter(times==50), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="README: \n 50 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_2<-ggplot(data %>% filter(times==100), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="README: \n 100 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_3<-ggplot(data %>% filter(times==500), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="README: \n 500 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))
full1<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))

## Case study 1
data<-df_cs1

p_r<-ggplot(data %>% filter(times==10), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1)+
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 1: \n 10 time steps ") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_1<-ggplot(data %>% filter(times==50), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 1: \n 50 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_2<-ggplot(data %>% filter(times==100), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 1: \n 100 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_3<-ggplot(data %>% filter(times==500), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 1: \n 500 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))
full2<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))



## Case study 2
data<-df_cs2

p_r<-ggplot(data %>% filter(times==10), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1)+
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 2: \n 10 time steps ") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_1<-ggplot(data %>% filter(times==50), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 2: \n 50 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_2<-ggplot(data %>% filter(times==100), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 2: \n 100 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))+
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_3<-ggplot(data %>% filter(times==500), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 2: \n 500 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12))
full3<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))


## Case study 3
data<-df_cs3

p_r<-ggplot(data %>% filter(times==10), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1)+
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 3: \n 10 time steps ") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_1<-ggplot(data %>% filter(times==50), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 3: \n 50 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_2<-ggplot(data %>% filter(times==100), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 3: \n 100 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))

p_3<-ggplot(data %>% filter(times==500), aes(x=individuals, y=(mean/60)))+
  geom_point(aes())+
  geom_smooth(method = lm, linewidth = 1, col = "blue") +
  labs(y="Run time in minutes", 
       x="Number of individuals", 
       title="Case study 3: \n 500 time steps") +
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(axis.text.x = element_text(vjust=0.6, size= 12)) +
  theme(axis.text.y = element_text(vjust=0.6, size= 12)) +
  theme(axis.title.y = element_text(vjust=0.6, size= 13)) +
  theme(axis.title.x = element_text(vjust=0.6, size= 13)) +
  theme(plot.subtitle = element_text(hjust=0.5, size= 12)) +
  scale_x_continuous(labels = c(0,1000,"",3000,"",5000))
full4<-plot_grid(p_r,p_1,p_2,p_3, nrow=1, ncol=4, align = "hv", scale=c(.98,.98, .98, .98))




## Combine all 4 plots 
plot_grid(full1,full2,full3,full4, nrow=4, ncol=1, align = "hv", scale=c(.98,.98, .98, .98)) 
## Export 10 x10 












