rm(list = ls())
library(tidyverse)
library(lubridate)

dat_segments <- read.csv("Raw/Segment.csv")
dat_regions <- read.csv("Raw/Region.csv")
dat_products <- read.csv("Raw/Products.csv")
dat_customers <- read.csv("Raw/Customers.csv")

dat_transactions <- read.csv("Raw/Transactions.csv") %>% head(4)

tst_customers <- dat_customers %>% head(20)

prop_days <- data.frame(
        Day = rep(c(1,2,3,4,5,6,7),2),
        Prop = c(c(15,3,1,1,1,5,12),c(3,6,8,8,8,4,2)),
        Demo = c(rep("Old", 7), rep("Regular", 7))
) %>% 
        mutate(
                Prop = (Prop / 38) * 1.75
        )
prop_days

prop_segs <- data.frame(
        SegmentID = rep(c(1,2,3,4),2),
        Prop = c(c(1,1,9,9),c(5,5,5,5)),
        Demo = c(rep("Old", 4), rep("Regular", 4))
) %>% 
        mutate(
                Prop = (Prop / 20) * 1.5
        )
prop_segs

prop_prod <- data.frame(
        ProductID = rep(c(1:12), 2),
        Prop = c(c(2,2,2,1,2,1,1,1,1,2,2,2),c(2,2,2,2,1,1,2,2,2,1,1,1)),
        Demo = c(rep("Old", 12), rep("Regular", 12))
) %>% 
        mutate(
                Prop = (Prop / 19) * 5
        )
prop_prod

dat_newtrans <- data.frame()
ydayshop <- 0

for (user in tst_customers$CustomerID) {
        if(dat_customers[dat_customers$CustomerID==user,]$CustomerAgeBand == "56+") {
                prop_days_group <- prop_days %>% 
                        filter(
                                Demo == "Old"
                        )
                prop_segs_group <- prop_segs %>% 
                        filter(
                                Demo == "Old"
                        )
                prop_prod_groups <- prop_prod %>% 
                        filter(
                                Demo == "Old"
                        )
                typical <- 8
        } else {
                prop_days_group <- prop_days %>% 
                        filter(
                                Demo == "Regular"
                        )
                prop_segs_group <- prop_segs %>% 
                        filter(
                                Demo == "Regular"
                        )
                prop_prod_groups <- prop_prod %>% 
                        filter(
                                Demo == "Regular"
                        )
                typical <- 11
        }
        
        for (i in c(1:31)) {
                shopdate <- date("2012-06-30") + days(i)
                shopday <- wday(shopdate)
                if (ydayshop == 0) {
                        if(prop_days_group[prop_days_group$Day==shopday,][2] >= runif(1,0,1)) {
                                tdidshop <- 1
                        } else {
                                tdidshop <- 0
                        }
                } else if (ydayshop == 1) {
                        tdidshop <- 0
                }
                ydayshop <- tdidshop
                dat_newtrans <- dat_newtrans %>% 
                        rbind(
                                data.frame(
                                        UserID = user,
                                        ShopDate = shopdate,
                                        ShopDay = shopday,
                                        didShop = tdidshop
                                )
                        )
        }
}

dat_newtrans <- dat_newtrans %>% 
        filter(
                didShop == 1
        ) %>% 
        select(
                UserID,
                ShopDate
        )
dat_newtrans$TransactionID <- c(1:length(dat_newtrans$UserID))

dat_newtrans %>% head()

for (tran_id in dat_newtrans$TransactionID) {
        
}



