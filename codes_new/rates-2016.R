
source('codes_new/CBR_web.R')
library(rusquant)
library(dygraphs)
library(ggplot2)
library(ggthemes)
library(tidyr)

start.date <- as.Date('2015-06-01')
end.date <- Sys.Date()

#Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")
#Sys.setlocale(category = "LC_ALL", locale = "C")


# скачать данные
mosprime <- MosPrimeXML(start.date,end.date)
saldo <- SaldoXML(start.date, end.date)
Ruonia <- RuoniaXML(start.date, end.date)
Depo <- DepoDynamicXML(start.date, end.date)
Repo <- RepoSessionXML(start.date, end.date)
miacr <- MKRXML(start.date, end.date)
repo.v <- REPOXML(start.date, end.date)
#unsec <- UnSecLoansXML(start.date, end.date)
#nonmarket <- NonMarketCreditXML(start.date, end.date)

aa <-by(repo.v[, c("avg_yield")], repo.v[,"Dt"], FUN = median )
repo.ir <- do.call(rbind, as.list(aa))
repo.ir <- xts(repo.ir, order.by = as.Date(row.names(repo.ir)))
repo.ir <- repo.ir[!(repo.ir == 0)]



# построить степ-чарт по ставкам
buffer <- na.locf(merge.xts(Ruonia$ruo, Depo$TomNext, Repo$Rate7Day, mosprime$T1W, repo.ir))
buffer <- na.locf(buffer, fromLast=TRUE)

names(buffer) <- c ('RUONIA', 'Standing depo', 'Standing repo (7 day)', 
                    'Mosprime 1w', 'Auction repo (7 days)' )


df <- xtsToDf(buffer)
df_long <- gather(df, ind, value, -date)
head(df_long)
p <- ggplot(data = df_long, aes(x = date, y = value, color = ind))
p <- p+geom_step() + theme_minimal(base_size =  8)+labs(x=NULL, y = '%')+scale_colour_manual(values=hse_colours)
p <- p+theme(legend.title = element_blank())+ theme(legend.position=c(.8, .8))
p <- p+scale_y_continuous(breaks=seq(9,20, by =0.5))

p
ggsave(filename = paste0('fig/rates - ', end.date, '.png'),width = 14, height=6.2, units = 'cm', dpi = 300)
ggsave(filename = paste0('fig/rates - ', end.date, '.emf'),width = 14, height=6.2, units = 'cm', dpi = 300)


#построить графики
dygraph(mosprime)
