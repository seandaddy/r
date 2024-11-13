library(readxl)
winr <- read_excel("winr.xlsx")
winrn <- winr[,c(1,7:28)]
winrate = reshape(data = winrn,
              idvar = "team",
              varying = c("winr1996", "winr1997",
                          "winr1998", "winr1999",
                          "winr2000", "winr2001",
                          "winr2002", "winr2003",
                          "winr2004", "winr2005",
                          "winr2006", "winr2007",
                          "winr2008", "winr2009",
                          "winr2010", "winr2011",
                          "winr2012", "winr2013",
                          "winr2014", "winr2015",
                          "winr2016", "winr2017"),
              sep = "",
              timevar = "year",
              times = c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,20010,2011,2012,2013,2014,2015,2016,2017),
              new.row.names= 1:660,
              direction = "long")
winrate$tm<-winrate$team
permin_all <- full_join(player_stats,winrate, by=c("tm","year"))

