rm(list = ls())
library(tidyverse)
library(lubridate)

dat_segments <- read.csv("Raw/Segment.csv")
dat_regions <- read.csv("Raw/Region.csv")
dat_products <- read.csv("Raw/Products.csv")
dat_customers <- read.csv("Raw/Customers.csv")

dat_transactions <- read.csv("Raw/Transactions.csv") %>% head(4)

dat_names <- read.csv("Raw/names.csv")
dat_names$CustomerID <- c(1:length(dat_names$First.Name))

dat_customers <- dat_names %>% 
        cbind(
                sample_n(dat_customers, size = 200, replace = T) %>% 
                        select(
                                CustomerValueSegment,
                                CustomerAgeBand
                        )
        ) %>% 
        mutate(
                CustomerName = paste0(First.Name, " ", Last.Name)
        ) %>% 
        select(
                CustomerID,
                CustomerName,
                CustomerValueSegment,
                CustomerAgeBand
        )

dat_customers

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

for (user in dat_customers$CustomerID) {
        print(paste0("Processing Customer ", user, ", ", dat_customers[dat_customers$CustomerID==user,]$CustomerName))
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
        
        for (i in c(1:(365*4))) {
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
baskets <- data.frame()

for (tran_id in dat_newtrans$TransactionID) {
        print(paste0("Processing transaction ID ", tran_id, " of ", max(dat_newtrans$TransactionID)))
        user <- dat_newtrans[dat_newtrans$TransactionID==tran_id,]$UserID
        if(dat_customers[dat_customers$CustomerID==user,]$CustomerAgeBand == "56+") {
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
        shopsize <- round(runif(1, min = 1, max = typical),0)
        tmp_segs <- data.frame()
        for (j in c(1:shopsize)) {
                tmp_segs <- tmp_segs %>% 
                        rbind(
                                prop_segs_group %>% 
                                        mutate(
                                                Prop = sapply(Prop, function(x) {runif(1, min = 0, max = x)[1]})
                                        ) %>% 
                                        arrange(
                                                desc(Prop)
                                        ) %>% 
                                        slice_head(n = 1) %>% 
                                        select(
                                                SegmentID
                                        )
                        )
        }
        tmpbasket <- prop_prod_groups %>% 
                sample_n(
                        size = shopsize,
                        replace = F
                )
        tmpbasket$SegmentID <- tmp_segs$SegmentID
        baskets <- baskets %>% 
                rbind(
                        tmpbasket %>% 
                                mutate(
                                        TransactionID = tran_id,
                                        Quantity = sapply(0.85, function(x) {ceiling(rgamma(12, x))[c(1:shopsize)]})
                                ) %>% 
                                select(
                                        TransactionID,
                                        ProductID,
                                        SegmentID,
                                        Quantity
                                )
                )
                
}

baskets

discount_schedule <- data.frame()

for (i in c(1:60)) {
        disc_month <- date("2012-06-01") + months(i)
        disc_count <- rpois(1,1)
        disc_total <- runif(disc_count, 0.05, 0.4)
        disc_prods <- dat_products %>% 
                sample_n(
                        size = disc_count,
                        replace = F
                )
        if (disc_count > 0) {
                discount_schedule <- discount_schedule %>% 
                        rbind(
                                data.frame(
                                        DiscountMonth = c(disc_month),
                                        ProductID = disc_prods$ProductID,
                                        DiscountTotal = disc_total
                                )
                        )
        }
}

discount_schedule

dat_newtrans <- dat_newtrans %>% 
        left_join(
                by = "TransactionID",
                baskets
        ) %>% 
        left_join(
                by = "ProductID",
                dat_products %>% 
                        select(
                                -SegmentID
                        ) %>% 
                        mutate(
                                StorePrice = c(
                                        1.95,
                                        2.29,
                                        2.49,
                                        0.69,
                                        0.89,
                                        1.29,
                                        1.89,
                                        0.29,
                                        0.39,
                                        2.49,
                                        4.09,
                                        6.99
                                ),
                                CostPrice = round(StorePrice * rnorm(12, 0.75, 0.1), 1) - 0.01
                        )
        ) %>% 
        mutate(
                StorePrice = ifelse(SegmentID == 1, StorePrice + ((ProductID %% 4) / 10) + 0.1, StorePrice),
                StorePrice = ifelse(SegmentID == 4, StorePrice + ((ProductID %% 3) / 10) + 0.05, StorePrice),
                CostPrice = ifelse(SegmentID == 1, CostPrice + ((ProductID %% 5) / 10) + 0.05, CostPrice),
                CostPrice = ifelse(SegmentID == 4, CostPrice + ((ProductID %% 4) / 20) + 0.05, CostPrice),
                DiscountMonth = as_date(paste0(str_sub(ShopDate, 1, 8), "01"))
        ) %>% 
        left_join(
                by = c("DiscountMonth", "ProductID"),
                discount_schedule
        ) %>% 
        mutate(
                DiscountTotal = ifelse(is.na(DiscountTotal), 1, 1 - DiscountTotal),
                StorePrice = StorePrice * DiscountTotal
        )

dat_newtrans %>% head()

