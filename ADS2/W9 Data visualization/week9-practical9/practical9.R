library(tidyverse)
#library("gridExtra")
#library(ggplot2)
library(cowplot)
#install.packages('ppubr')
#library(ppubr)
#dev.off()
#dev.new()
data(diamonds)
#data=diamonds
#head(diamonds)
#g=ggplot(data=diamonds,mapping=
#         aes(x=carat,y=price,group=cut))
#g
#g1=g+geom_point(stat="identity",
#                aes(colour=cut))
#g1=g+geom_point(stat="identity",
#                aes(colour=cut),size=0.1,alpha=0.2)
#g1=g+geom_point(stat="identity",
#                aes(colour=cut),size=0.1,alpha=0.2,shape=18)
#alpha:transparency
#d=ggplot(diamonds,aes(carat))+xlim(0,3)
#d+stat_bin(aes(y=after_stat(density)),
#           binwidth=0.1,geom="area")
#png(file="fig2.png")
#d+stat_bin(aes(y=..count..),
#           binwidth=0.01,geom="area")
#d+stat_bin(aes(size=..density..),
#           binwidth=0.1,geom="point",
#           )
#png(file="purple.png")
#d+geom_area(stat="count")
g=ggplot(data=diamonds,mapping=
          aes(x=clarity,y=carat,color=cut))
g=g+geom_boxplot()
g=g+labs(title="Carat vs clarity")
#g=g+ggtitle("Carat vs clarity")
#g=g+
sm=geom_smooth(method="lm",mapping=aes(x=as.numeric(clarity),
                                       y=clarity))
#g=g+facet_wrap(cut~color)
#g=g+facet_grid(cut~color)
#g=g+scale_color_brewer(rgb(red=1,green=0.1,blue=1))
g=g+scale_color_brewer(palette="Purples")
g1=g+theme(plot.title=element_text(hjust=0.5)) #title in the middle

#task4
#g=g+scale_y_log10()
g=g+scale_y_continuous(trans="log10")
#g=g+scale_y_continuous(trans="log10",name="log10 carot")
g=g+labs(y="carot/log10(carat)")
#g=g+theme()
#g=g+theme(ylab="log10 carat")

#g
#g1=g+sm
#g1
g1=g+geom_smooth(method="lm",mapping=aes(x=as.numeric(clarity)))
g2=g+scale_y_continuous(limit=c(0.3,3.0))
#ggarrange(g1,g2)
plot_grid(g1,g2)
#dev.off()