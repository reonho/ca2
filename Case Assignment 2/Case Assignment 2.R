####QUESTION 1####

#before policy implemented
bef_pol <- append(DAD$`2011`[1:8],DAD$`2010`, after = 0)
bef_pol_ts <- ts(bef_pol)
plot(bef_pol_ts)
# upward trend, possibly seasonal
#do we need training and testing data?? 2010 model to forecast and check if 2011 is correctly predicted?
bef_pol_hw <- HoltWinters(bef_pol_ts, gamma = F) #why does this not work
plot(bef_pol_hw)
library("forecast")
bef_pol_fc <- forecast(bef_pol_hw)
plot(bef_pol_fc)
bef_pol_hw
bef_pol_fc

#after policy implemented
