# assume that myData_NA and myData_noNA are in workspace
# this code preprocesses the data for all available weeks to generate final predictions
library(plyr)

# group1 <- data.frame(household_key=numeric(), transaction_freq=numeric(), quantity_freq=numeric(), 
#                      quantity_per_purchase=numeric(), avg_price=numeric(), avg_price_per_item=numeric(),
#                      avg_discount=numeric())

myData_NA$COMMODITY_DESC <- as.factor(myData_NA$COMMODITY_DESC)
myData_noNA$COMMODITY_DESC <- as.factor(myData_noNA$COMMODITY_DESC)

group1_1 <- data.frame() # households for which optional data is not  provided
day_cutoff = 704
# for each household, calculate our predictors and append to the group1_1 data frame
for(household in levels(myData_NA$household_key)) {
    sub_df = myData_NA[myData_NA$household_key==household & myData_NA$DAY<=day_cutoff,]
    transaction_freq = nrow(sub_df) / (day_cutoff-499)
    quantity_freq = sum(sub_df$QUANTITY) / (day_cutoff-499)
    quantity_per_purchase = sum(sub_df$QUANTITY) / nrow(sub_df)
    avg_price = mean(sub_df$NET_SPEND_AMT)
    avg_price_per_item = sum(sub_df$NET_SPEND_AMT) / sum(sub_df$QUANTITY)
    avg_discount = -sum(sub_df$COUPON_DISC)
    
    new_df = data.frame(household_key=household, transaction_freq=transaction_freq, quantity_freq=quantity_freq, 
                        quantity_per_purchase=quantity_per_purchase, avg_price=avg_price, avg_price_per_item=avg_price_per_item, 
                        avg_discount=avg_discount)
    for(department in levels(myData_NA$DEPARTMENT))
        new_df[[department]] = sum(sub_df$DEPARTMENT==department) / nrow(sub_df)
    for(desc in levels(myData_NA$COMMODITY_DESC)) # turns out this information is not very useful
        new_df[[desc]] <- sum(sub_df$COMMODITY_DESC==desc) / nrow(sub_df)
    new_df[["egg_purchase_freq"]] = sum(sub_df$COMMODITY_DESC=="EGGS") / (day_cutoff-499)
    new_df[["days_since_egg_purchase"]] = day_cutoff - max(c(500, max(sub_df[sub_df$COMMODITY_DESC=="EGGS",]$DAY)))
    new_df[["days_since_transaction"]] = day_cutoff - max(sub_df$DAY)
    group1_1 <- rbind.fill(group1_1, new_df)
}
# detemine whether eggs were purchased during the last week
for(household in group1_1$household_key) {
    sub_df = myData_NA[myData_NA$household_key==household & myData_NA$DAY>day_cutoff,]
    group1_1[["purchased_eggs"]][which(group1_1$household_key==household)] = sum(sub_df$COMMODITY_DESC=="EGGS")>0
}

# do the same for group 2
group2_1 <- data.frame() # households with optional data
for(household in levels(myData_noNA$household_key)) {
    sub_df = myData_noNA[myData_noNA$household_key==household & myData_noNA$DAY<=day_cutoff,]
    transaction_freq = nrow(sub_df) / (day_cutoff-499)
    quantity_freq = sum(sub_df$QUANTITY) / (day_cutoff-499)
    quantity_per_purchase = sum(sub_df$QUANTITY) / nrow(sub_df)
    avg_price = mean(sub_df$NET_SPEND_AMT)
    avg_price_per_item = sum(sub_df$NET_SPEND_AMT) / sum(sub_df$QUANTITY)
    avg_discount = -sum(sub_df$COUPON_DISC)
    
    new_df = data.frame(household_key=household, transaction_freq=transaction_freq, quantity_freq=quantity_freq, 
                        quantity_per_purchase=quantity_per_purchase, avg_price=avg_price, avg_price_per_item=avg_price_per_item, 
                        avg_discount=avg_discount)
    for(department in levels(myData_noNA$DEPARTMENT))
        new_df[[department]] = sum(sub_df$DEPARTMENT==department) / nrow(sub_df)
    for(desc in levels(myData_NA$COMMODITY_DESC))
        new_df[[desc]] <- sum(sub_df$COMMODITY_DESC==desc) / nrow(sub_df)
    for(opt in c("AGE_DESC", "MARITAL_STATUS_CODE", "INCOME_DESC", "HOMEOWNER_DESC",
                   "HH_COMP_DESC", "HOUSEHOLD_SIZE_DESC", "KID_CATEGORY_DESC")) {
        new_df[[opt]] = sub_df[[opt]][1]
    }
    new_df[["egg_purchase_freq"]] = sum(sub_df$COMMODITY_DESC=="EGGS") / (day_cutoff-499)
    new_df[["days_since_egg_purchase"]] = day_cutoff - max(c(500, max(sub_df[sub_df$COMMODITY_DESC=="EGGS",]$DAY)))
    new_df[["days_since_transaction"]] = day_cutoff - max(sub_df$DAY)
    group2_1 <- rbind.fill(group2_1, new_df)
}
# determine whether eggs were purchased in the last week
for(household in group2_1$household_key) {
    sub_df = myData_noNA[myData_noNA$household_key==household & myData_noNA$DAY>day_cutoff,]
    group2_1[["purchased_eggs"]][which(group2_1$household_key==household)] = sum(sub_df$COMMODITY_DESC=="EGGS")>0
}

# OPTIONAL: this code creates baskets, but was later found to be not useful
group1_1baskets <- list()
for(household in levels(myData_NA$household_key)) {
    sub_df = myData_NA[myData_NA$household_key==household & myData_NA$DAY<=day_cutoff,]
    indiv_basket = unique(sub_df$COMMODITY_DESC)
    group1_1baskets[[household]] <- list(indiv_basket)
}
group2_1baskets <- list()
for(household in levels(myData_noNA$household_key)) {
    sub_df = myData_noNA[myData_noNA$household_key==household & myData_noNA$DAY<=day_cutoff,]
    indiv_basket = unique(sub_df$COMMODITY_DESC)
    group2_1baskets[[household]] <- list(indiv_basket)
}