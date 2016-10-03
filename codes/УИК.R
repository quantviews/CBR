require(XML)
require(RCurl)
require(stringr)
require(RJSONIO)
require(RColorBrewer)

url <- 'http://www.moscow_city.vybory.izbirkom.ru/region/region/moscow_city?action=show&root=1&tvd=27720001368293&vrn=27720001368289&region=77&global=&sub_region=77&prver=0&pronetvd=null&vibid=27720001368293&type=222'
urldoc <- htmlTreeParse(url, useInternalNodes = T)

links <- xpathSApply(urldoc, "//a[contains(@style, 'TEXT-DECORATION: none')]", xmlAttrs ) 
t(links)
links <- as.data.frame(t(links))
links <- as.character(links[,2])

nm <- names(buff)
buff <- as.data.frame(matrix(nrow =0, ncol = length(nm), dimnames = list(NULL, 
                                                               nm))) 
i <- 1
for (i in 1:length(links)){
  url <- links[i]
  cat(url)
  doc <- getURL(url)
  doc <- iconv(doc, "windows-1251", "UTF-8")
  doc <- sub("windows-1251", "UTF-8", doc)
  tables <- readHTMLTable(doc)
  table <- as.data.frame(tables[9])
  table <- apply(table, MARGIN=2, as.character)
  
  uik.id <- as.character(unlist(str_extract_all((table[1,]), "[0-9]+")))
  Degtyarev <- as.character(unlist(str_extract_all(table[20,], "[0-9]{1,2}.[0-9]{1,2}%")))
  Levichev <- as.character(unlist(str_extract_all(table[21,], "[0-9]{1,2}.[0-9]{1,2}%")))
  Melnikov <-  as.character(unlist(str_extract_all(table[22,], "[0-9]{1,2}.[0-9]{1,2}%")))
  Mitrokhin <- as.character(unlist(str_extract_all(table[23,], "[0-9]{1,2}.[0-9]{1,2}%")))
  Navalny <- as.character(unlist(str_extract_all(table[24,], "[0-9]{1,2}.[0-9]{1,2}%")))
  Sobyanin <- as.character(unlist(str_extract_all(table[25,], "[0-9]{1,2}.[0-9]{1,2}%")))
  
  buff <- rbind(buff, data.frame(uik.id, Degtyarev, Melnikov, Mitrokhin, Navalny, Sobyanin))
}

write.csv(buff, file = 'data/moscow-elections-2013.csv')
names(buff) <- c('NUMBER', "Дегтярев", "Мельников", "Митрохин", "Навальный", "Собянин")
uik2 <- merge(buff, uik, by= "NUMBER")

require(devtools)
install_github('rCharts', 'ramnathv', 'dev')
require(rCharts)


uik <- read.csv(file='data/uik.csv')
width = 1600 
height = 800

map3 <- Leaflet$new()
map3$set(width = width, height = height)
map3$setView(c(55.7512, 37.6184), zoom = 12)
map3$tileLayer(provider = 'Stamen.TonerLite')

names(uik2)[11:12] <- c('latitude', 'longitude')
uik2 <- apply(uik2, 2, as.character)
uik2 <- as.data.frame(uik2)
uik2$popup <- '<p>Retreived At: </p>'
uik2$fillColor <- "#D7191C"

rownames(uik2) <- as.character(uik2$NUMBER)
uik_lst <- split(uik2, rownames(uik2))
uik_list <- lapply(uik_lst, as.list)
ui4<- lapply(uik_list, as.character)

apply(uik2, 1, function(x) {as.list(uik2[x,], all.names=TRUE)})
apply(uik2, 1, function(x) {x})
uik3 <- list()
for (i in 1:nrow(uik2)){
 uik3 <- append(uik3, as.list(uik2[i,], all.names=TRUE))
}
library(rjson)
uik3 <- apply(uik2, 1, toJSON)
uik_list<- fromJSON(uik3)
uik3 <- apply(uik2, 1, function(r) paste(names(uik2), r, sep=":", collapse=" "))

circleData <- uik[,c("LAT", "LONG")]
names(circleData) <- c('lat', 'lng')
circleData$radius = 500
circleData$fillColor <- '#f03'
circleData$color <- 'red'

map3$circle2(circleData)

i <- 2
for (i in 1:nrow(uik)){
  cat(i)
  map3$circle(LatLng = c(uik[i,'LAT'], uik[i,'LONG']))
              bindPopup = paste0("<p> УИК",  uik[i,1],  "(", uik[,2], "</p>"))
}
map3$geoJson(toGeoJSON(uik_lst), 
           onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.popup)
    } !#',
           pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: 4,
        fillColor: feature.properties.fillColor || 'red',    
        color: '#000',
        weight: 1,
        fillOpacity: 0.8
      })
    } !#")

map3
map3$publish()

getNetworks <- function(){
  require(httr)
  if (!file.exists('networks.json')){
    url <- 'http://api.citybik.es/networks.json'
    dat <- content(GET(url))
    writeLines(dat, 'networks.json')
  }
  networks <- RJSONIO::fromJSON('networks.json')
  nms <- lapply(networks, '[[', 'name')
  names(networks) <- nms
  return(networks)
}
network = 'citibikenyc'
data_ <- getData(network);
center_ <- getCenter(network, networks)

getCenter <- function(nm, networks){
  net_ = networks[[nm]]
  lat = as.numeric(net_$lat)/10^6;
  lng = as.numeric(net_$lng)/10^6;
  return(list(lat = lat, lng = lng))
}

getData <- function(network = 'citibikenyc'){
  require(httr)
  url = sprintf('http://api.citybik.es/%s.json', network)
  bike = fromJSON(content(GET(url)))
  lapply(bike, function(station){within(station, { 
    fillColor = cut(
      as.numeric(bikes)/(as.numeric(bikes) + as.numeric(free)), 
      breaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), 
      labels = brewer.pal(5, 'RdYlGn'),
      include.lowest = TRUE
    ) 
    popup = iconv(whisker::whisker.render(
      '<b></b><br>
        <b>Free Docks: </b>  <br>
         <b>Available Bikes:</b> 
        <p>Retreived At: </p>'
    ), from = 'latin1', to = 'UTF-8')
    latitude = as.numeric(lat)/10^6
    longitude = as.numeric(lng)/10^6
    lat <- lng <- NULL})
  })
}

