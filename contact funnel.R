#FUNNEL VIEW
f.0 <- read.csv("L:/Product_Design/_Reference_Files/Brendan/Curtis PS Universe 10-26-15.csv")

#LOCATIONID
nrow(f.0[is.na(f.0$Location.ID),])
f.1 <- f.0[!is.na(f.0$Location.ID),]

#BROKERS
nrow(f.1[is.na(f.1$Brokers.At.Site),])
f.2 <- f.1[!is.na(f.1$Brokers.At.Site),]

#COSTAR
nrow(f.2[!is.na(f.2$Has.Costar),])
f.3 <- f.2[is.na(f.2$Has.Costar),]

#PS
nrow(f.3[f.3$Has.PS.Corp==0 & f.3$Has.PS.ECom==0,])
f.4 <- f.3[f.3$Has.PS.Corp==1 | f.3$Has.PS.ECom==1,]


f.df <- data.frame(filter=c("None","LocationID","Has Brokers At Site","Has Costar", "Has Premium Searcher"),
                   lost=c(0,nrow(f.0[is.na(f.0$Location.ID),]),nrow(f.1[is.na(f.1$Brokers.At.Site),]),nrow(f.2[!is.na(f.2$Has.Costar),]),nrow(f.3[f.3$Has.PS.Corp==0 & f.3$Has.PS.ECom==0,])),
                   remaining=c(nrow(f.0),nrow(f.1),nrow(f.2),nrow(f.3),nrow(f.4))
)

write.csv(f.df, file = "C:/Users/bfreehart/Documents/20151028 LoopNet Subscribers/20151117 Saved Plots & Tables/contact funnel.csv")

#nrow(ps[ps$Has.PF.Corp==1 | ps$Has.PF.ECom==1,])
#nrow(ps[ps$Has.PC.Corp==1 | ps$Has.PC.ECom==1,])
#nrow(ps[ps$Lease.Listings>=1 | ps$Sale.Listings>=1,])
