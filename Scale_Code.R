scale.data<-read.csv(file.choose(),header=TRUE)
head(scale.data)
#creating a new dataframe with specific columns if needed
library(reshape2)
myvars<-c("ind","com","eco","ind.com","ind.eco","comm.eco","all")
levels<-c("levels")
newdata<-scale.data[myvars]
newdata2<-scale.data[levels]
melted<-melt(newdata)
melted2<-melt(newdata2)

##main plot
library(ggplot2)
main<-ggplot(melted,aes(variable,value,na.rm=TRUE))  + 
  geom_bar(stat="identity",color=I("grey20"),fill=I("grey20")) + 
  scale_x_discrete(labels=c("ind"="Individual","com"="Community","eco"="Ecosystem","ind.com"="Individual + Community","ind.eco"="Individual. + Ecosystem","comm.eco"="Community + Ecosystem","all"="Individual + Community + Ecosystem")) +
  ylab("Number of studies") +
  theme(
    axis.text.x = element_text(color='black',size=11,angle=45,hjust=1),
    axis.text.y = element_text(color="black",size=10),
    axis.title.x=element_blank(),legend.position="none",
    #axis.text.y=element_blank(),axis.ticks=element_blank(),
    #axis.text.x=element_blank(),axis.ticks=element_blank(),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=17),
    panel.background = element_rect(fill=NA,color='black'),
    panel.grid = element_blank())


##inset plot
percent<-ggplot(melted2, aes(x = factor(value))) +  
  geom_bar(aes(y = (..count..)/sum(..count..)),color=I("grey20"),fill=I("grey20")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Number of levels assessed") +
  ylab("Percentage of studies") +
  theme(
    axis.text = element_text(color='black',size=12),
    #axis.text.y=element_blank(),axis.ticks=element_blank(),
    #axis.text.x=element_blank(),axis.ticks=element_blank(),
    axis.title = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=17),
    panel.background = element_rect(fill=NA,color='black'),
    panel.grid = element_blank())

##where you positiont the inset graph on the big graph
vp <- viewport(width = 0.35, height = 0.35, x = 0.79,y=0.77)
y = unit(0.7, "lines"), just = c("right",
                                 "bottom")

#executing the inset, you create a function the utlizes all the previous code
library(grid)

full <- function() {
  print(main)
  print(percent, vp = vp)
}
full()
