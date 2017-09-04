library(ggplot2)
library(reshape2)

myData <- read.csv("transaction_data_8451.csv", stringsAsFactors = FALSE)

str(myData)
# our data frame has 287506 rows and 19 cols

# convert certain columns to factors
cols_to_factorize = c("AGE_DESC", "MARITAL_STATUS_CODE", "INCOME_DESC", "HOMEOWNER_DESC",
                      "HH_COMP_DESC", "HOUSEHOLD_SIZE_DESC", "KID_CATEGORY_DESC", "DEPARTMENT")
myData[, cols_to_factorize] <- lapply(myData[, cols_to_factorize], as.factor)

# look at missing values
na_count_col <- sapply(myData, function(col) sum(is.na(col)))
na_count_col
# We see that for each of AGE_DESC through KID_CATEGORY_DESC (7 columns),
# there are exactly 117352 NA values, leading us to suspect that there are
# simply 117352 rows that don't contain data for these labels.

na_count_row <- apply(myData, 1, function(row) sum(is.na(row)))
table(na_count_row)
# yup, either all 7 optional factors are included in a row, or they're all excluded
# 170154 rows include the optional data, 117352 exclude it, based on household.

# introduce PRICE_PER_PRODUCT
myData$PRICE_PER_PRODUCT <- myData$NET_SPEND_AMT / myData$QUANTITY
summary(myData$PRICE_PER_PRODUCT)
# there are rows w/ QUANTITY == 0, resulting in Inf values (n / 0), and NaN (0 / 0)
# we'll give these rows a 0 value for PRICE_PER_PRODUCT
nrow(myData[myData$QUANTITY == 0,]) # there are 647 such rows
myData[myData$QUANTITY==0,]$PRICE_PER_PRODUCT = 0
summary(myData$PRICE_PER_PRODUCT)

# look at distributions of spending & discounts for rows w/ and w/o NA values
myData_noNA <- myData[!is.na(myData$AGE_DESC),] # group with optional values
myData_noNA$household_key <- as.factor(myData_noNA$household_key)
myData_NA <- myData[is.na(myData$AGE_DESC),] # group without optional values
myData_NA$household_key <- as.factor(myData_NA$household_key)
myData$household_key <- as.factor(myData$household_key)

sumsummary(myData$NET_SPEND_AMT)
ggplot() + geom_density(aes(x=NET_SPEND_AMT), colour="red", data=myData_noNA) + 
    geom_density(aes(x=NET_SPEND_AMT), colour="blue", data=myData_NA) + 
    coord_cartesian(xlim = c(0, 30))
# total spending looks the same

summary(myData$COUPON_DISC)
ggplot() + geom_density(aes(x=-COUPON_DISC), colour="red", data=myData_noNA[myData_noNA$COUPON_DISC!=0,]) + 
    geom_density(aes(x=-COUPON_DISC), colour="blue", data=myData_NA[myData_NA$COUPON_DISC!=0,]) + 
    coord_cartesian(xlim = c(0, 5))
# coupon discounts look similar as well, when savings aren't 0


# look at distributions of the exclusive data