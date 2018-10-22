####QUESTION 1####

#Defects after delivery 
#before policy implemented on Aug 2011
library(reshape2)
DAD_melted <- melt(DAD)
bef_pol <-DAD_melted[1:20,]
bef_pol_ts <- ts(bef_pol$value)
plot(bef_pol_ts)
res = cbind(bef_pol, c(1:20))

# upward, seasonal trend. Use multiple linear regression
for(month in bef_pol$Month[2:12]){
  if (month == "January"){
  }
  else{
    col <- sapply(bef_pol$Month, function(x) ifelse(x == month, 1, 0))
    res = cbind(res, col)
  }
}
colnames(res) <- c("month", "year", "value","Time", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov","Dec")
aov_bef_pol <- aov(value ~ (Time + Feb + Mar + Apr+ May + Jun+ Jul+ Aug+ Sep+ Oct+Nov+Dec), data = res)
summary(aov_bef_pol)
lm_bef_pol <- lm(value ~ (Time + Feb + Mar + Apr+ May + Jun+ Jul+ Aug+ Sep+ Oct+Nov+Dec), data = res)
summary(lm_bef_pol)
#going nowhere 


#???? should we just use holtwinters to predict??

#employee retention
ER_numeric <- ER
ER_numeric$Local <- sapply(ER$Local, function(x) ifelse(x == "Y", 1,0))
ER_numeric$Gender <- sapply(ER$Gender, function(x) ifelse(x == "M",1,0))
ER_numeric$`College Grad` <- sapply(ER$`College Grad`, function(x) ifelse(x=="Y",1,0))
lm_ER <- lm(YearsPLE ~ YrsEducation + `College Grad` + `College GPA` + Age + Gender + Local, data = ER_numeric)
summary(lm_ER)
coefficients(lm_ER)
#Diagnostic plots
plot(lm_ER)


#rate of learning
plot(ENG)
#R2 Calc
tss <- sum((ENG$`Production Time (min)`-mean(ENG$`Production Time (min)`))^2)
rsq <- function(x){
  ess <- sum(residuals(x)^2)
  return(1 - ess/tss)
}
#Linear model
ENG_linear <- lm(ENG$`Production Time (min)`~ ENG$Sample)
summary(ENG_linear)
abline(ENG_linear)
#Exponential model
expo <- function(x,a,b){
  b*a^x
}
ENG_exp <- nls(formula = ENG$`Production Time (min)`~expo(ENG$Sample,a,b), data = ENG, start = list(a=1, b=1))
ENG_expco <- coefficients(ENG_exp)
curve(expo(x, ENG_expco[1], ENG_expco[2]), from = ENG$Sample[1], to = ENG$Sample[50], add = T, lwd =2)
rsq(ENG_exp) #0.872
#Logarithmic model
logo <- function(x,a,b){
  a*log(x) +b 
}
ENG_log <- nls(ENG$`Production Time (min)`~ logo(ENG$Sample,a,b), data = ENG, start = c(a =1, b=1))
ENG_logco <- coefficients(ENG_log)
curve(logo(x, ENG_logco[1], ENG_logco[2]), from = 1, to = 50, add = T, lwd =2)
rsq(ENG_log) #0.996
#Polynomial Model
poly <- function(x,a,b,c){
  c + a*x + I(b*x^2)
}
ENG_poly <- nls(ENG$`Production Time (min)`~ poly(ENG$Sample,a,b,c), data = ENG, start = c(a =1, b=1, c =1))
ENG_polyco <- coefficients(ENG_poly)
curve(poly(x, ENG_polyco[1], ENG_polyco[2], ENG_polyco[3]), from = 1, to = 50, add = T, lwd =2)
rsq(ENG_poly) #0.961


