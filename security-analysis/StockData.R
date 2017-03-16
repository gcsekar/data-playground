# Check and Install Packages

CheckInstalledPackages= function(package.name){
  
 if (!package.name %in% rownames(installed.packages())) {install.packages(package.name, dependencies = TRUE)}
 #install.packages(package.name, dependencies = TRUE)
}

LoadPackages = function(package.name){
  library(package.name, character.only = TRUE)
}

packages = c("xts","rvest","pbapply","TTR","dygraphs","lubridate")

lapply(X = packages,FUN = CheckInstalledPackages)
lapply(X = packages, FUN = LoadPackages)

stock.list<-'https://www.loyal3.com/stocks'
stocks<-read_html(stock.list)
stocks.names<-html_nodes(stocks,'.company-name')
stocks.names<-html_text(stocks.names)

#
#
loyal.links<-html_nodes(stocks, "a")
loyal.links<-html_attr(loyal.links, "href")
stock.links<-paste0('http://www.loyal3.com',loyal.links[54:123])

#
#

get.ticker<-function(url){
  x<-read_html(url)
  x<-html_node(x,'.ticker-price')
  x<-html_text(x)
  x<-sub("^([[:alpha:]]*).*", "\\1", x)
  return(x)
} 

#
#

stock.tickers<-pblapply(stock.links,get.ticker)
#
#

stock.ticks<-do.call(rbind,stock.tickers)
stock.ticks<-data.frame(symbol=stock.ticks,name=stocks.names)

start.date<-Sys.Date()
end.date<-Sys.Date()-years(3)

start.date<-gsub('-','', start.date)
end.date<-gsub('-','', end.date)

#
#
stocks.ts<-pblapply(stock.ticks$symbol,getYahooData,end.date, start.date)


mov.avgs<-function(stock.df){
  stock.close<-stock.df[,4]
  ifelse((nrow(stock.df)<(2*260)),
         x<-data.frame(stock.df, 'NA', 'NA'),
         x<-data.frame(stock.df, SMA(stock.close, 200), SMA(stock.close, 50)))
  colnames(x)<-c(names(stock.df), 'sma_200','sma_50')
  x<-x[complete.cases(x$sma_200),]
  return(x)
}

dygraph(stocks.ts$AUPH[,c('Close','sma_200','sma_50')],main = 'AUPH Moving Averages') %>%
  dySeries('Close', label='Close') %>%
  dySeries('sma_50', label = 'sma 50') %>%
  dySeries('sma_200', label = 'sma 200') %>%
  dyRangeSelector(height = 30) %>%
  dyShading(from = '2016-4-28', to = '2016-7-27', color = '#CCEBD6') %>%
  dyShading(from = '2016-7-28', to = '2016-12-30', color = '#FFE6E6')


