library(RODBC)
library(ggplot2)
library(plyr)
library(dplyr)

con <- odbcConnect('209 Adhoc')

q <- paste0("SELECT DISTINCT USA.session_id
	          , USA.userid
            , CASE WHEN PSA.Product IN ('LDCF','LDCFC','LDCFU') THEN 'LA' WHEN PSA.Product IN ('LCP','LCPUK','LCPCA') THEN 'LC' ELSE 'EE' END AS Product
            , USA.login_time
            , USA.last_access
            , CAST(DATEDIFF(s, USA.login_time, USA.last_access) AS DECIMAL(12, 4)) / 60 AS duration
            FROM ProductLoginSessions.dbo.User_Sessions_Archive (NOLOCK) AS USA
            INNER JOIN ProductLoginSessions.dbo.Product_Sessions_Archive (NOLOCK) AS PSA ON PSA.Session_id = USA.Session_id
            LEFT JOIN Enterprise.dbo.WebUsers AS wu (NOLOCK) ON wu.UserId = USA.userid
            LEFT JOIN Enterprise.dbo.Contact AS c (NOLOCK) ON wu.UserContactID = c.ContactID
            LEFT JOIN Enterprise.dbo.Location AS l (NOLOCK) ON l.LocationID = c.LocationID
            WHERE USA.login_time >= '01/01/2015'
            AND PSA.Product IN 
            (
            'LDCF','LDCFC','LDCFU' --Lease Analysis
            ,'LCP','LCPUK','LCPCA'  --Lease Comps
            )
            AND CAST(DATEDIFF(s, USA.login_time, USA.last_access) AS DECIMAL(12, 4)) / 60 > 1
            AND l.LocationName NOT LIKE '%CoStar%'"
)

logins <- sqlQuery(con, q)

logins <- mutate(logins, login.date = as.Date(logins$login_time), login.week = as.integer(format(as.Date(logins$login_time)+3, "%U")))

#ggplot(logins, aes(x=duration)) +
#  geom_density(colour='blue',fill='blue', alpha=0.3) +
#  xlab("Duration (Mins)") +
#  scale_x_continuous(breaks=1:20, limits=c(0,20))


#daily.login <- ddply(logins[logins$Product=='LA',], c("login.date"), summarise,
#                 login.count = n()
#)

#ggplot(daily.login, aes(x=login.date, y=login.count)) +
#  geom_line() +
#  ylab("Logins") 


weekly.login <- ddply(logins, c("Product", "login.week"), summarise,
                     login.count = n()
)

#save(weekly.login, file = "weekly.login")

#sapply(logins, typeof)

ggplot(weekly.login[weekly.login$Product=='LA',], aes(x=login.week, y=login.count)) +
  geom_line() +
  ggtitle("Lease Analysis Logins per Week - 2015") +
  ylab("Logins") +
  xlab("Week") +
  xlim(0,41)

#ggsave(file="C:/Users/bfreehart/Documents/20150901 Lease Analytics & Lease Comps Usage/lease analysis logins per week 2015.10.19.png", width=6, height=4)

ggplot(weekly.login[weekly.login$Product=='LC',], aes(x=login.week, y=login.count)) +
  geom_line() +
  ggtitle("Lease Comps Logins per Week - 2015") +
  ylab("Logins") +
  xlab("Week") +
  xlim(0,41)

#max(weekly.login[weekly.login$Product=='LA',]$login.week)

