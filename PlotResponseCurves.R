# Plot directed connectivity response curves
# for documentation figure (Appendix C)

library(ggplot2)
library(reshape)
library(RColorBrewer)

#Define minimum proportion of target value remaining for each curve
min1 = 0.75
min2 = 0.5
min3 = 0.25

#Define increments in proportion upstream neighborhood lost
x = seq(0,1,by=0.5)

#Define y-values of response curves
y0 = rep(1,length(x)) #flat curve - no directed connectivity considered
y1 = seq(1,min1,by=-(1-min1)/(length(x)-1))
y2 = seq(1,min2,by=-(1-min2)/(length(x)-1))
y3 = seq(1,min3,by=-(1-min3)/(length(x)-1))

#Create data frame
df <- data.frame(x,y0,y1,y2,y3)
colnames(df) <- c("x","No benefits","Small benefits","Moderate benefits","Large benefits")
dfm <- melt(df,id="x")

#Plot figure
ggplot(data=dfm, aes(x=x, y=value,group=variable)) +
  geom_line(aes(color=variable)) +
  geom_point(aes(color=variable)) + 
  scale_color_manual(name="Downstream benefits",values=c("#104E8B","#1874CD","#1C86EE","#63B8FF")) + 
  scale_y_continuous(limits=c(0,1.0)) +
  labs(x="Proportion upstream neighborhood 'lost'",y="Proportion focal unit value remaining") +
  #scale_color_discrete(name="Downstream benefits") +
  theme_minimal()