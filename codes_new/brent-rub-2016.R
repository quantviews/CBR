# построить график соотношение цен на нефть и курса рубля

source('codes_new/CBR_web.R')
library(rusquant)
library(dygraphs)
library(ggplot2)
library(ggthemes)

start.date <- as.Date('2015-01-01')
end.date <- Sys.Date()

#скачать данные 
brent <- getSymbols('ICE.BRN', src='Finam', from=start.date, symbol.lookup = FALSE)
brent <- ICE.BRN
usd <- (GetCursDynamicXML(start.date, end.date, 'R01235')) #доллар
eur <- (GetCursDynamicXML(start.date, end.date, 'R01239')) #евро
basket <- (merge(usd, eur)) # объединить вместе


df <- merge.xts(brent[,4], usd)
names(df) <- c('brent', 'usd')
df <- df[complete.cases(df)]

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
p <- p+annotate('point', x = last.brent, y = last.usd,  colour = "red", size = 5)+
  annotate('text', x = last.brent, y = last.basket, 
           label = paste0(' ', last.date), vjust = 2, colour= 'red')
p <- p + theme(legend.title = element_blank()) + theme(legend.position=c(.8, .8))
p<- p +scale_x_continuous(breaks=seq(30,110, by =5))+scale_y_continuous(breaks=seq(45,85, by =5))
p <- p +  scale_colour_manual(values=hse_colours)

p


ggsave(filename = paste0('fig/brent-rub - ', end.date, '.png'),width = 14, height=6.2, units = 'cm', dpi = 300)
ggsave(filename = paste0('fig/brent-rub - ', end.date, '.emf'),width = 14, height=6.2, units = 'cm', dpi = 300)
