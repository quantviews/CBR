# построить график соотношение цен на нефть и курса рубля

source('codes_new/CBR_web.R')

#if(!require(rusquant)){nstall.packages("rusquant", repos="http://R-Forge.R-project.org")};require(rusquant)
if(!require(dygraphs)){install.packages("dygraphs")};require(dygraphs)
if(!require(ggplot2)){install.packages("ggplot2")};require(ggplot2)
if(!require(xts)){install.packages("xts")};require(xts)
if(!require(ggthemes)){install.packages("ggthemes")};require(ggthemes)
library(rusquant)

### some additional stuff
hse_colours <- c('#3255a4', '#9a3324', '#ff8038', '#55688b', '#4daa50', '#55688b','#ff8038')

# преобразовать объект xts в dataframe с сохранением индекса даты
XtstoDf <- function(ts, ...){ 
  df <- as.data.frame(ts)
  df$date <- time(ts)
  return(df)
}

### main code 

start.date <- as.Date('2014-01-01')
end.date <- Sys.Date()
#скачать данные 

brent <- rusquant::getSymbols.Finam('ICE.BRN', from=start.date, symbol.lookup = FALSE) # Брент на ICE
wti <- rusquant::getSymbols.Finam('NYMEX.CL', from=start.date) # Контракт WTI на NYMEX
getSymbols('SPFB.BR', src='Finam', from=start.date, symbol.lookup = FALSE) # Контракт Брент на MOEX

usd <- (GetCursDynamicXML(start.date, end.date, 'R01235')) #доллар
eur <- (GetCursDynamicXML(start.date, end.date, 'R01239')) #евро
basket <- (merge(usd, eur)) # объединить вместе
basket$basket <- basket$eur * 0.45 + basket$usd * 0.55 # бивалютная корзина
basket.m <- aggregate((basket), as.yearmon, mean, na.rm=TRUE ) # среднемесячные значения для курсов 
copy.table(XtstoDf(basket.m))
copy.table(XtstoDf(basket))
basket.y <- aggregate((basket), as.year, mean, na.rm=TRUE ) # среднемесячные значения для курсов 

df <- merge.xts(brent[,4], usd)
names(df) <- c('brent', 'usd')
df <- df[complete.cases(df)]

# some other charts if you need 
#chartSeries(brent, theme	= 'white')
#dygraph(usd,main = 'RUB/USD', ylab = NA )
#dygraph(eur,main = 'RUB/USD', ylab = NA )

buffer <- xtsToDf(df)
buffer$year <- format.Date(buffer$date, '%Y')
buffer$year <- as.factor(buffer$year)

last.usd <- as.numeric(round(last(df[,2]),2))
last.brent <- as.numeric(round(last(df[,1]),2))
last.date <- last(index(df))

#построиь график
p <- ggplot(data = buffer, aes(x=brent, y = usd, col = year))
p<- p + geom_point()+stat_smooth(method = 'lm', formula = y ~ log(x))+theme_minimal(base_size = 8)

p <- p <- p+labs(x=' Brent, $/barrel', y= 'RUB/USD')
p <- p+annotate('point', x = last.brent, y = last.usd,  colour = "red", size = 4)+
  annotate('text', x = last.brent, y = last.basket, 
           label = paste0(' ', last.date), vjust = 1, colour= 'red')
p <- p + theme(legend.title = element_blank()) + theme(legend.position=c(.8, .8))
p<- p +scale_x_continuous(breaks=seq(30,110, by =5))+scale_y_continuous(breaks=seq(45,85, by =5))
p <- p +  scale_colour_manual(values=hse_colours)

p


ggsave(filename = paste0('fig/brent-rub - ', end.date, '.png'),width = 14, height=7, units = 'cm', dpi = 300)
ggsave(filename = paste0('fig/brent-rub - ', end.date, '.emf'),width = 14, height=6.2, units = 'cm')


# log returns and rolling correlations

if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")};require(PerformanceAnalytics)


head(df)
ret <- Return.calculate(df[,1],method = 'log') # сaculate returns
ret <- merge(ret, Return.calculate(1/df[,2],method = 'log'))

ret <- ret[-1,]

plot(as.numeric(ret[,1]), as.numeric(ret[,2]))
chart.Scatter(ret[,1],ret[,2], xlab = 'Brent return', ylab = 'RUB return', main = 'Brent and RUB return')
abline(lm(ret[,2]~ret[,1]), col="red") # regression line (y~x) 
cor(lag(ret[,1],0), lag(ret[,2],0), use = 'complete.obs')
cor(lag(ret[,1],1), lag(ret[,2],0), use = 'complete.obs')

plot.xts(ret)


ret <- ret['2015::']
chart.RollingCorrelation(ret[,1], ret[,2],width = 20, main = 'Rolling correlation of RUB/USD and Brent returns ', ylab = NA,)
chart.RollingCorrelation(lag(ret[,1],1), ret[,2],width = 20, main = 'Rolling correlation of RUB/USD and Brent returns ', ylab = NA)

library(ggfortify)

autoplot(ret$brent, ts.geom = 'bar', fill = 'blue')
autoplot(ret$usd, ts.geom = 'bar', fill = 'blue')
autoplot(ret, ts.geom = 'bar', scales = 'fixed' )
