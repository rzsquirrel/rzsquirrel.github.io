# IMPORTANT: first run proprocess_final to obtain final training data for models

# train final models

group1_preds <- predict(rf1_1,group1_1,type="prob")
group2_preds <- predict(rf2_1,group2_1,type="prob")
names(group1_preds) <- c("no","yes")
names(group2_preds) <- c("no","yes")

final_preds_group1 <- as.data.frame(cbind(as.numeric(as.character(group1_1$household_key)),group1_preds$yes))
names(final_preds_group1) <- c("household_key","probability")
final_preds_group2 <- as.data.frame(cbind(as.numeric(as.character(group2_1$household_key)),group2_preds$yes))
names(final_preds_group2) <- c("household_key","probability")

final_preds <- rbind(final_preds_group1,final_preds_group2)
final_preds <- final_preds[order(final_preds$household_key),]
str(final_preds)

#write to output file
write.csv(final_preds, file = "kittens_are_cute.csv", row.names = FALSE)
