# расчет общей задолженности по валютному репо 

DateFrom <- as.Date('2014-10-01')
DateTo <- as.Date(Sys.Date())

df <- REPOFXXML(DateFrom, DateTo)
last_date <- max(df$snd_date)

# создать пустой xts от первой даты до итоговой 
fx_repo <- xts(rep(0,length.out = as.numeric(last_date - DateFrom)+1), 
               order.by = seq.Date(DateFrom, last_date,by = 1))
names(fx_repo) <- 'stock'
#проверить есть ли не-долларовые сделки 
if(all(df$Currency == 'USD')){
    for (i in 1:length(df$Dt)) {
      #определить временной интервал каждой сделки
      deal_length <- (seq.Date(df[i,'first_date'], df[i,'snd_date'], by =1)) 
      #добавить сумму к "пустому" xts - fx_repo
      fx_repo[deal_length] <- fx_repo[deal_length] + df[i,'avg_deal']
      
    }
  }  
  else{
    print("Есть сделки не в долларах")
  }
chart.TimeSeries(fx_repo/1000,ylab = 'bln $',main = 'Общая задолженность банков по операциям валютного репо')

