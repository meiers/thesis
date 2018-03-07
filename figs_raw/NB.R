library(ggplot2)
library(data.table)
p = 0.4
x = seq(1,30)
y1 = dnbinom(x, 9.5*p/(1-p), p)
y2 = dnbinom(x,   5*p/(1-p), p)
y3 = dnbinom(x, 0.5*p/(1-p), p)
ggplot(data.table(x=rep(x,3),y=c(y1,y2,y3),t=c(rep("2N",30),rep("N",30),rep("0",30)))) + 
    aes(x,y,col=t) + geom_line(size=1) + theme_minimal() + 
    xlab("Number of reads") + ylab("NB density") + 
    ggtitle("NB with 10 reads per bin") + theme(legend.position = "bottom") + 
    scale_color_discrete(name = NULL)


x = seq(1,100)
y1 = dnbinom(x, 47.5*p/(1-p), p)
y2 = dnbinom(x,   25*p/(1-p), p)
y3 = dnbinom(x,  2.5*p/(1-p), p)
ggplot(data.table(x=rep(x,3),y=c(y1,y2,y3),t=c(rep("2N",100),rep("N",100),rep("0",100)))) + 
    aes(x,y,col=t) + geom_line(size=1) + theme_minimal() + 
    xlab("Number of reads") + ylab("NB density") + 
    ggtitle("NB with 50 reads per bin") + theme(legend.position = "bottom") + 
    scale_color_discrete(name = NULL)
