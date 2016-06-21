# Построение кумулятивных графиков с динамикой факторов ликвидности по РФ


Sys.getlocale(category = "LC_ALL") #LC_COLLATE=Russian_Russia.1251
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")


source('codes_new/CBR_web.R')

library(dplyr)
require(tidyr)


start.date <- as.Date('2013-01-01')
end.date <- Sys.Date()

mon <- c('I', 'II', "III", "IV", "V", "VI", "VII", 
         "VIII", "IX", "X", "XI", "XII")


liq <- FlikvidXML(start.date, end.date)
liq$ostatki <- as.numeric(liq$ostatki)
liq$year <- (as.POSIXlt(liq$date))$year + 1900
liq$year <- as.factor(liq$year)
liq$day <- as.POSIXlt(liq$date)$yday # порядковый номер дня в году (0-365)
liq$month <- as.POSIXlt(liq$date)$mon # порядковый номер месяца (0-11)
liq$day <- as.factor(liq$day)

head(liq)

liq <- (tidyr::gather(liq,id,value, -year, -day,-date,-month))
liq <- tbl_df(liq)


liq$value <- as.numeric(liq$value)
liq$id <- (as.factor(liq$id))

by_liq <- liq %>% #посчитать накопленную сумму по годам
  group_by(id, year) %>%
  mutate(cumsum=cumsum(value))


levels(liq$id)
id2 <- 'cash'
id2 <- 'govt'
id2 <- 'liq'
id2 <-   "reserve.req"


buff <- dplyr::filter(by_liq, id ==id2)
buff <- group_by(buff, year,date)
buff

#bb<- plyr::ddply(liq, .(year), summarize, value <- cumsum(cash), day <- day)
#names(bb) <- c('year', 'value', 'day')

p <- ggplot(data = buff, aes(x = as.numeric(day), y = cumsum, group = year))
p <- p+geom_line(aes(colour = year), size = 1)+theme_minimal(base_size = 8)+
  scale_x_continuous(breaks=seq(0, 359, by = 30), labels = mon)+
  labs(x = '', y = 'bln RUB', colour = 'year')
p

ggsave(paste0('fig/liq/', id2, '.png'), width = 14, height=6.2, units = 'cm', dpi = 300)

id2 <- 'ostatki'
buff <- dplyr::filter(by_liq, id ==id2)

buff <- buff %>% 
  group_by(year, month) %>%
  summarise(value = mean(value))

p <- ggplot(data = buff, aes(x = as.numeric(month)+1, y = value, group = year))
p <- p+geom_line(aes(colour = year), size = 1)+theme_minimal(base_size = 8)+
  scale_x_continuous(breaks=seq(1, 12, by = 1), labels = mon)+
  labs(x = '', y = 'bln RUB', colour = 'year')
p

ggsave(paste0('fig/liq/', id2, '.png'), width = 14, height=6.2, units = 'cm', dpi = 300)


plot.xts(brent)
years <- 2010:2013
ts <- brent

#Функция. Построение сезонного графика по годам 
SeasonalityDaily <- function(ts, years = 2010:2013, ylab =''){
    ts.df <- as.data.frame(ts)
    ts.df$date <- time(ts)
    ts.df$year <- format(time(ts), '%Y')
    ts.df$DayOfYear <- as.numeric( format(time(ts), '%j'))
    ts.df$month <- format(time(ts), '%b')
    current.year <- as.numeric(format(Sys.Date(), '%Y'))
    ts.df <- subset(ts.df, year %in% years)
    ts.df$alpha_line = ifelse(ts.df$year == current.year, 1, 0.5)
    p <- ggplot(ts.df, aes_string(x = 'DayOfYear', y = names(ts), group = 'year'))
     p<- p + geom_line(aes(color = year, alpha = as.numeric(year)/current.year), size = 1)+labs(x = '', color = '', y = ylab)+ief.theme+
      scale_colour_manual(values = rev(ief.palette[1:length(years)]))+scale_alpha(guide = 'none')
    p +  scale_x_discrete(breaks = round(c(2, (1:11)*30.4375),0), 
                                  labels=c("янв","фев","март","апр","май", 'июнь', 'июль', 'авг', 'сент', 'окт', 'ноя', 'дек'))
}
(1:12)*30
SeasonalityDaily(brent)