library(data.table)
library(curl)
library(XML)
library(dplyr)

load("C:/Users/bfreehart/Documents/20150812 Fraud Theft Detection/UsageAnalyticsData20150930.Rda")

bad_user <- d[c("locationid","userid","ip_address","CookieCount","ExportedRows")]

isp <- data.frame("Location"=NA, "IP Address"=NA,"ISP"=NA,"Name"=NA,"PostalCode"=NA
                  )
n <- length(bad_user[,3])

StartTime <- Sys.time() #Start Timing
for(i in 1:n) {
  url <- toString(bad_user[i,3])
  
  xml_raw <- readLines(curl(url = paste0("http://whois.arin.net/rest/ip/",url)))
  
  xml_data <- xmlToList(xml_raw)
  
  orgRef  <- as.data.frame(xml_data[["orgRef"]])
  nm      <- as.data.frame(xml_data[["name"]])
  pc  <- as.data.frame(xml_data[["postalCode"]])
  
  isp <- rbind(isp,c(bad_user[i,1],url,toString(orgRef[1,2]),toString(nm),toString(pc)))
}
RunTime <- as.numeric(Sys.time()-StartTime)
print(paste("API Loop took",round(RunTime,2),"min")) #14 min

#save(isp,file="C:/Users/bfreehart/Documents/20150812 Fraud Theft Detection/ISPData.Rda")
