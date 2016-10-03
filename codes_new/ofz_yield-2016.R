# данные 

source('codes_new/CBR_web.R')
if(!require(rusquant)){nstall.packages("rusquant", repos="http://R-Forge.R-project.org")};require(rusquant)

library(tidyr)

start.date <- as.Date('2016-01-01')
end.date <- Sys.Date()


OnDate <- as.Date('2016-01-01')
ToDate <- Sys.Date()

rates <- Isoterm(OnDate,ToDate,I_Day = 2)

rates1 <- Isoterm(start.date,end.date,I_Day = 1)
rates2 <- Isoterm(start.date,end.date,I_Day = 2)

rates <- merge(rates1, rates2)
names(rates) <- c('y1', 'y2')
rates$y1y1 <- ((rates$y2+100)/100)^2/((rates$y1+100)/100)*100-100


plot.xts(rates[,c('y1', 'y1y1')], legend.loc = 'topright')
rates2
plot.xts(rates2)

copy.table(xtsToDf(rates))


OnDate <- last(index((rates1)))
curve <- GCurve(OnDate)
curve$m1 <- GCurve(OnDate-30)$val
curve$m2 <- GCurve(OnDate-60)$val
curve$m3 <- GCurve(OnDate-91)$val

curve <- curve[curve$year<15,]

names(curve) <- c('year',as.character(c(OnDate, OnDate-30, OnDate-60, OnDate-91)))
curve <- gather(curve,date,value, -year)

p <- ggplot(curve, aes(x = year, y = value, color = date))
p  + geom_line()+theme_minimal(base_size = 8) + scale_x_continuous(breaks=seq(1,14, by =1))+labs(y='%')

ggsave(paste0('fig/', 'ofz_yields', '.png'), width = 14, height=6.2, units = 'cm', dpi = 300)

###

getSymbols('XS0564087541', src='Finam', from=start.date, symbol.lookup = FALSE) # Контракт WTI на NYMEX
