library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)

#READ IN BARCLAY PREMIUM SERCHER REPORT
ps.raw <- read.csv("L:/Product_Design/_Reference_Files/Brendan/Curtis PS Universe 10-26-15.csv")

ps.raw <- ps.raw[!is.na(ps.raw$Location.ID),] #FILTER MISSING LOCATIONS

#READ/TRANSFORM IN MAPPING TABLES
CityTier <- read.csv("C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/CoStar - Active VM Sheet - CityTier ResearchMarket.csv") #Market->Price Tier MAPPING

z <- read.csv("C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/zip_to_market_mapping.csv", colClasses = "factor") #Market->Zip Code MAPPING

CSCost <- read.csv("C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/Costar - Active VM Sheet - Cost.csv") #Price Tier->Cost MAPPING

z.CityTier <- merge(z,CityTier,by.x = c("ResearchMarket"),by.y = c("City"), all = TRUE) #CITY->ZIP

z.CityTier[is.na(z.CityTier)] <- 3 #REPLACE UNLINKED ZIPCODES WITH 3rd TIER

#MAP PRICING TIERS TO PS FILE
ps <- merge(ps.raw,CityTier,by.x = c("Real.Research.market"),by.y = c("City"), all=TRUE) #APPEND PRICING TIER BY RESEARCH MARKET

ps$Tier <- z.CityTier$Tier[match(ps$postalzipcode, z.CityTier$ZipCode)] #SET PRICING TIER BY ZIP CODE

ps$Tier[is.na(ps$Tier)] <- 3 #SET PRICING TIER = 3 FOR UNMAPPED ROWS

#MAP CS COST TO PS FILE
ps <- merge(ps,CSCost,by.x = c("Tier","Brokers.At.Site"),by.y = c("Tier","Users")
                   , all.x=TRUE
                   ) #PREMIUM SEARCHER DATA->COST

#HARDCODE FORMULA TO UPDATE ESTIMATED COSTS FOR 50+ BROKERS
ps <- mutate(ps, Suite = ifelse(Tier==1 & Brokers.At.Site>50, 229.58*Brokers.At.Site + 1165.1, Suite))
ps <- mutate(ps, X2Product = ifelse(Tier==1 & Brokers.At.Site>50, 180*Brokers.At.Site + 920.85, X2Product))
ps <- mutate(ps, X1Product = ifelse(Tier==1 & Brokers.At.Site>50, 138.2*Brokers.At.Site + 250.76, X1Product))
ps <- mutate(ps, Suite = ifelse(Tier==2 & Brokers.At.Site>50, 245.69*Brokers.At.Site + 337.25, Suite))
ps <- mutate(ps, X2Product = ifelse(Tier==2 & Brokers.At.Site>50, 192.63*Brokers.At.Site + 267.18, X2Product))
ps <- mutate(ps, X1Product = ifelse(Tier==2 & Brokers.At.Site>50, 143.11*Brokers.At.Site + 65.741, X1Product))
ps <- mutate(ps, Suite = ifelse(Tier==3 & Brokers.At.Site>50, 199*Brokers.At.Site, Suite))

ps <- ps[!is.na(ps$Brokers.At.Site),]

#SET 3rd TIER CONTACTS TO 0 FOR 1&2 PRODUCT
ps$X1Product[is.na(ps$X1Product)] <- 0 
ps$X2Product[is.na(ps$X2Product)] <- 0
#SET NA LISTINGS TO 0 FOR IMPUTING VARIABLES
ps$Lease.Listings[is.na(ps$Lease.Listings)] <- 0
ps$Sale.Listings[is.na(ps$Sale.Listings)] <- 0
ps$CoStar.Site.REvenue[is.na(ps$CoStar.Site.REvenue)] <- 0

#FUNCTION AGGREGATES AND CHARTS BY RESEARCH MARKET
p.opp.mkt <- function(ps.x) {

  ps.loc <- unique(ps.x[,c("Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","CoStar.Site.REvenue","X1Product","X2Product","Suite")])
  
  #CALCULATE ADDITIONAL FIELDS
  ps.loc <- mutate(ps.loc
                   , adj.1Product = X1Product - CoStar.Site.REvenue
                   , adj.2Product = X2Product - CoStar.Site.REvenue
                   , adj.Suite = Suite - CoStar.Site.REvenue
  )
  
  #SET NEGATIVE REVENUE OPPORTUNITY TO 0
  ps.loc$adj.1Product[ps.loc$adj.1Product<0] <- 0
  ps.loc$adj.2Product[ps.loc$adj.2Product<0] <- 0
  ps.loc$adj.Suite[ps.loc$adj.Suite<0] <- 0
  
  opp.mkt <- ddply(ps.loc, c("Real.Research.market"), summarise, Min.Opp = sum(adj.1Product), Max.Opp = sum(adj.Suite))
  
  #ORDER DESCENDING BY THE MAX OPPORTUNITY
  opp.mkt <- opp.mkt[order(-opp.mkt$Max.Opp),]
  
  #APPEND CUMULATIVE SUM PERCENTILE
  opp.mkt <- mutate(opp.mkt, cum_prcnt_sum = opp.mkt$Max.Opp/cumsum(opp.mkt$Max.Opp))
  
  #RENAME 'Other' FOR MARKETS WITH < 1% OF CUMULATIVE OPPORTUNITY
  opp.mkt <- transform(opp.mkt, Real.Research.market = as.character(Real.Research.market))
  opp.mkt[opp.mkt$cum_prcnt_sum<0.01, "Real.Research.market"] <- "Other"
  
  #REAGGREGATE OPPORTUNITY
  opp.mkt <- ddply(opp.mkt, c("Real.Research.market"), summarise, Min.Opp = sum(Min.Opp), Max.Opp = sum(Max.Opp))
  
  #SET MAX AND MIN TO CUMULATIVELY ADD TO MAX (FOR STACKED COLUMN CHART)
  opp.mkt <- mutate(opp.mkt, Max.Opp = Max.Opp - Min.Opp)
  
  opp.mkt1 <- melt(opp.mkt, id.vars = "Real.Research.market")
  
  #CHART BY DECENDING VALUE
  opp.p <- ggplot(opp.mkt1[!is.na(-opp.mkt1$value),], aes(x=reorder(Real.Research.market,-value), y=value, fill=variable)) +
    geom_bar(stat="identity",alpha=0.7) +
    scale_fill_brewer(palette = "Set1",
                      name="Opportunity",
                      labels=c("1 Product","Suite")
    ) +
    xlab("Market") + ylab("Monthly Opportunity $") +
    #ggtitle("CoStar Opportunity") +
    theme(axis.text.x = element_text(angle = 90,vjust=0, hjust = 1)) +
    scale_y_continuous(labels= dollar)
  
  return(opp.p)
  
}

#FUNCTION FOR WIDE CRITERIA - SENSITIVITY ANALYSIS W/ LOOP SEARCH AS CUT
sens.wid <- function(s) {
  ps.c <- ps[is.na(ps$Has.Costar) #Do Not Have CoStar
             & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) #Uses Premium Searcher
             & (ps$Loop.Searches.Past.180.Days>s) #Loop Searches Cut
             , c("Tier","Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","Lease.Listings","X1Product","X2Product","Suite","Total.Run.Rate","CoStar.Site.REvenue","Loop.Searches.Past.180.Days")
             ]
  
  #AGGREGATE BY LOCATION IN ORDER TO CALCULATE ADJUSTED OPPORTUNITY
  ps.loc <- unique(ps.c[,c("Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","CoStar.Site.REvenue","X1Product","X2Product","Suite")])
  
  ps.loc <- mutate(ps.loc, adj.Suite = Suite - CoStar.Site.REvenue) #CALCULATE ADJUSTED OPPORTUNITY
  
  ps.loc$adj.Suite[ps.loc$adj.Suite<0] <- 0 #SET NEGATIVE REVENUE OPPORTUNITY TO 0
  
  ps.loss <- sum(
      ps[is.na(ps$Has.Costar) & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) & (ps$Loop.Searches.Past.180.Days<=s), "Total.Run.Rate"]
  )
  
  return(data.frame(searches=s,ln.users.w=nrow(ps.c),tot.opp.w=sum(ps.loc$adj.Suite),loss.w=ps.loss,net.opp.w=sum(ps.loc$adj.Suite)-ps.loss))
}

#FUNCTION FOR NARROW CRITERIA - SENSITIVITY ANALYSIS W/ LOOP SEARCH AS CUT
sens.nrw <- function(s) {
  ps.c <- ps[is.na(ps$Has.Costar) #Do Not Have CoStar
             & (ps$Has.PF.Corp==1 | ps$Has.PF.ECom==1 | ps$Has.PC.Corp==1 | ps$Has.PC.ECom==1) #Uses Property Facts or Property Comps
             & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) #Uses Premium Searcher
             & (ps$Lease.Listings>=1 | ps$Sale.Listings>=1) #Has CoStar Listings
             & (ps$Loop.Searches.Past.180.Days>s) #Loop Searches Cut
             , c("Tier","Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","Lease.Listings","X1Product","X2Product","Suite","Total.Run.Rate","CoStar.Site.REvenue","Loop.Searches.Past.180.Days")
             ]
  
  #AGGREGATE BY LOCATION IN ORDER TO CALCULATE ADJUSTED OPPORTUNITY
  ps.loc <- unique(ps.c[,c("Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","CoStar.Site.REvenue","X1Product","X2Product","Suite")])
  
  ps.loc <- mutate(ps.loc, adj.Suite = Suite - CoStar.Site.REvenue) #CALCULATE ADJUSTED OPPORTUNITY
  
  ps.loc$adj.Suite[ps.loc$adj.Suite<0] <- 0 #SET NEGATIVE REVENUE OPPORTUNITY TO 0
  
  ps.loss <- sum(
    ps[is.na(ps$Has.Costar) & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) & (ps$Loop.Searches.Past.180.Days<=s), "Total.Run.Rate"]
  )
  
  return(data.frame(searches=s,ln.users.n=nrow(ps.c),tot.opp.n=sum(ps.loc$adj.Suite),loss.n=ps.loss,net.opp.n=sum(ps.loc$adj.Suite)-ps.loss))
}

#SUBSET BY NARROW AND WIDE FILTER
ps.nrw <- ps[is.na(ps$Has.Costar) #Do Not Have CoStar
             & (ps$Has.PF.Corp==1 | ps$Has.PF.ECom==1) #Uses Property Facts
             & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) #Uses Premium Searcher
             & (ps$Lease.Listings>=1 | ps$Sale.Listings>=1) #Has CoStar Listings
             , c("Tier","Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","Lease.Listings","X1Product","X2Product","Suite","Total.Run.Rate","CoStar.Site.REvenue","Loop.Searches.Past.180.Days")
             ]

ps.wid <- ps[is.na(ps$Has.Costar) #Do Not Have CoStar
             & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) #Uses Premium Searcher
             , c("Tier","Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","Lease.Listings","X1Product","X2Product","Suite","Total.Run.Rate","CoStar.Site.REvenue","Loop.Searches.Past.180.Days")
             ]

ps.nrw.c <- ps[is.na(ps$Has.Costar) #Do Not Have CoStar
             & (ps$Has.PF.Corp==1 | ps$Has.PF.ECom==1 | ps$Has.PC.Corp==1 | ps$Has.PC.ECom==1) #Uses Property Facts or Property Comps
             & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) #Uses Premium Searcher
             & (ps$Lease.Listings>=1 | ps$Sale.Listings>=1) #Has CoStar Listings
             & (ps$Loop.Searches.Past.180.Days>150) #Loop Searches Cut
             , c("Tier","Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","Lease.Listings","X1Product","X2Product","Suite","Total.Run.Rate","CoStar.Site.REvenue","Loop.Searches.Past.180.Days")
             ]

ps.wid.c <- ps[is.na(ps$Has.Costar) #Do Not Have CoStar
             & (ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1) #Uses Premium Searcher
             & (ps$Loop.Searches.Past.180.Days>150) #Loop Searches Cut
             , c("Tier","Real.Research.market","Location.ID","Brokers.At.Site","Number.of.Contacts.at.Site","Lease.Listings","X1Product","X2Product","Suite","Total.Run.Rate","CoStar.Site.REvenue","Loop.Searches.Past.180.Days")
             ]

#SUMMARIZE % COVERAGE OF ALL CRITERIA
d <- nrow(ps)
smry <- c(Do.Not.Have.CoStar = nrow(ps[is.na(ps$Has.Costar),]) / d, 
  Uses.Property.Facts = nrow(ps[ps$Has.PF.Corp==1 | ps$Has.PF.ECom==1,]) / d, 
  Uses.Property.Comps = nrow(ps[ps$Has.PC.Corp==1 | ps$Has.PC.ECom==1,]) / d, 
  Uses.Premium.Searcher = nrow(ps[ps$Has.PS.Corp==1 | ps$Has.PS.ECom==1,]) / d, 
  Has.CoStar.Listings = nrow(ps[ps$Lease.Listings>=1 | ps$Sale.Listings>=1,]) / d
)
smry <- melt(smry)

#PLOT USAGE AL
usage.p <- ggplot(ps.wid[ps.wid$Loop.Searches.Past.180.Days>0,], aes(x=Loop.Searches.Past.180.Days)) +
  geom_histogram(binwidth=10, colour="blue",fill="blue", alpha=0.3) + 
  xlab("Loop Searches - Past 180 Days") + ylab("Count of Contacts") +
  ggtitle("Premium Searcher Usage") +
  geom_vline(xintercept = c(50,100,150,200), colour="red") +
  xlim(0,1000)

#RUN SENSITIVITY ANALYSIS
t.wid <- rbind(sens.wid(50),sens.wid(100),sens.wid(150),sens.wid(200))
t.nrw <- rbind(sens.nrw(50),sens.nrw(100),sens.nrw(150),sens.nrw(200))
t <- merge(t.wid, t.nrw, by="searches")

#PLOT OPPORTUNITY
p.wid <- p.opp.mkt(ps.wid)
p.nrw <- p.opp.mkt(ps.nrw)
p.wid.c <- p.opp.mkt(ps.wid.c)
p.nrw.c <- p.opp.mkt(ps.nrw.c)

#SAVE TABLES & CHARTS
write.csv(smry, file = "C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/table contact summary.csv")
ggsave(file="C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/plot usage.png",plot=usage.p,width=6,height=6)
write.csv(t, file = "C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/table sensitivity analysis.csv")
ggsave(file="C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/plot wide.png",plot=p.wid,width=10,height=6)
ggsave(file="C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/plot narrow.png",plot=p.nrw,width=10,height=6)
ggsave(file="C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/plot wide cut.png",plot=p.wid.c,width=10,height=6)
ggsave(file="C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/plot narrow cut.png",plot=p.nrw.c,width=10,height=6)
