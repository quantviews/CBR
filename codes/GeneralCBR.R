# данные Банка России
# Источник: cbr.ru

require(XML)
url = 'http://www.cbr.ru/statistics/credit_statistics/MS.asp'
tables = readHTMLTable(url, encoding = "UTF-8")
doc = htmlParse(url, encoding = "UTF-8")
z = as.data.frame(readHTMLTable(doc, stringsAsFactors = FALSE)$lista)
table <- tables[2]

url <- 'http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=02/03/2010&date_req2=14/03/2011&VAL_NM_RQ=R01235'

if(url.exists('http://www.cbr.ru/secinfo/secinfo.asmx')) {
   POST /secinfo/secinfo.asmx HTTP/1.1
   Host: www.cbr.ru
   Content-Type: text/xml; charset=utf-8
   Content-Length: length
   SOAPAction: "http://web.cbr.ru/REPOXML"
   
   <?xml version="1.0" encoding="utf-8"?>
      <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
      <soap:Body>
      <REPOXML xmlns="http://web.cbr.ru/">
      <DateFrom>dateTime</DateFrom>
      <DateTo>dateTime</DateTo>
      </REPOXML>
      </soap:Body>
      </soap:Envelope>
      
wsdl = processWSDL("~/Projects/org/omegahat/XML/SOAP/examples/ndfdXMLserver.wsdl")
   