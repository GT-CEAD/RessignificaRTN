set.seed(1)

#my.n <-5000
my.n <-224

my.model <- list(ar=0.5)

my.sd<-1

my.ts <- arima.sim(n=my.n, model = my.model, sd= my.sd)

library(ggplot2)

temp.df <- data.frame(y=unclass(my.ts),date= Sys.Date()+1:my.n)

p<- ggplot(temp.df,aes(x=date, y=y))
p <- p + geom_line(size=0.5)

print(p)
