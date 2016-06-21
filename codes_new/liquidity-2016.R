source('codes_new/CBR_web.R')

library(dygraphs)
#
start.date <- as.Date('2015-01-01')
end.date <- Sys.Date()


# Данные по депозитам 

depo1 <- OstatDepoXML(start.date, end.date)
plot.xts(depo1/10^3, legend.loc='top')

ostat1 <- OstatDynamicXML(start.date, end.date)
ostat1 <- (ostat1[,-2])
depo <- merge(depo1[,3], ostat1)

dygraph(depo/1000000) %>%
  dySeries("total", label = "Депозиты") %>%
  dySeries("InRuss", label = "Корсчета") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

dygraph(depo/1000000) %>%
  dySeries("total", label = "Депозиты") %>%
  dySeries("InRuss", label = "Корсчета") %>%
  dyRangeSelector(height = 20)

dygraph(depo/1000000) %>%
  dySeries("total", label = "Депозиты") %>%
  dySeries("InRuss", label = "Корсчета") %>%
  dyRoller(rollPeriod = 25)
  dyRangeSelector(height = 20)

a <- DVXML(start.date, end.date)
copy.table(xtsToDf(a))

l1 <- DVXML(start.date, end.date)

plot.xts(l1/10^6, legend.loc = 'top', main = 'Требования ЦБ к банкам, млрд руб')
copy.table(xtsToDf(l1))

l2 <- Repo_debtXML(start.date, end.date)
tail(l2)
copy.table(xtsToDf(l2))
plot.xts(l2/10^6, legend.loc = 'top', main = 'Операции прямого репо, млрд руб')


l3 <- RepoDebtUSDXML(start.date, end.date)
tail(l3)
usd <- (GetCursDynamicXML(start.date, end.date, 'R01235')) #доллар
tail(l3)
tail(usd)
l3 <- l3[l3$TP ==1]
l3 <- na.locf(merge.xts(l3, usd,join = 'left'))
l3$debtSumRUB <- l3$debtSum * l3$usd
copy.table(xtsToDf(l3))
