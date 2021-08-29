
if (!require(ISLR)) install.packages("ISLR")
library(ISLR)

install.packages ("dplyr")
suppressMessages(library(dplyr))

if (!require(MASS)) install.packages("MASS")
library(MASS)

if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics") 
library(PerformanceAnalytics) 


if (!require(xts)) install.packages("xts")

summary(Auto)
auto <- data.frame(Auto)
head(auto)
model = lm(formula = mpg ~ horsepower, data = auto)
summary(model)
################################################################################

summary(mtcars)
m <- data.frame(mtcars)
m$am <- as.factor(m$am)
m$vs <- as.factor(m$vs)
m <- m %>%
  mutate(vs_hp= ifelse(vs==1,hp*1, hp*0))

head(m)
summary(m)

model1 <- lm(formula = mpg ~ hp, data = m)
summary(model1)

model2 <- lm(formula = mpg ~ hp+am+vs, data = m)
summary(model2)

model3 <- lm(formula = mpg ~ hp+am+vs+vs_hp, data = m)
summary(model3)
################################################################################

?Boston
head(Boston)
bos <- Boston %>%
  mutate(result=ifelse(medv>30,1,0))

head(bos)

Model1 <- glm(result ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat , data = bos, family = "binomial")
summary(Model1)

bos <-  bos %>% 
  mutate(pred_prob_model4 = predict(Model1, newdata = ., type = "response")) %>% 
  mutate(pred_outcome_model4 = ifelse(pred_prob_model4 >= 0.5,1,0))

head(bos)
xtabs(~result + pred_outcome_model4, data = bos)

################################################################################
setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
abalone  <- read.csv("abalone.csv")
head(abalone)

model1 <- lm(Rings ~ Diameter+Height , data = abalone)
summary(model1)
################################################################################
head(abalone)

abalone_male <- abalone %>%
  filter(Type %in% c('M','I')) %>%
  mutate(did=ifelse(Type=="I",0,1))

abalone_female <- abalone %>%
  filter(Type %in% c('F','I'))%>%
  mutate(did=ifelse(Type=="I",0,1))

head(abalone_male)
head(abalone_female)

#Method1
model1 <- lm(Diameter ~did, data=abalone_male)
summary(model1)

model2 <- lm(Diameter ~did, data=abalone_female)
summary(model2)

#Method2
a = sapply(subset(abalone_male, did == 0, select=Diameter), mean)
b = sapply(subset(abalone_male, did == 1, select=Diameter), mean)

b-a

a = sapply(subset(abalone_female, did == 0, select=Diameter), mean)
b = sapply(subset(abalone_female, did == 1, select=Diameter), mean)

b-a
################################################################################
setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
admission  <- read.csv("Admissions.csv")
head(admission)

model1 <- glm(Admitted ~., data=admission, family = "binomial" )
summary(model1)

admission <-  admission %>% 
  mutate(pred_prob_model1 = predict(model1, newdata = ., type = "response")) %>% 
  mutate(pred_outcome_model1 = ifelse(pred_prob_model1 > 0.75,1,0))
head(admission)
xtabs(~Admitted + pred_outcome_model1, data = admission)
(219+133)/(219+133+9+39)
################################################################################
?managers
data(managers)
head(managers)
tail(managers)

apply(managers,2,sd)

data <- as.data.frame(managers) %>%
  rename(us10 = `US 10Y TR`) %>%
  rename(SP500=`SP500 TR`)

data <- data %>%
  mutate(MktExcess = SP500 - us10) %>%
  mutate(FundExcess_ham1 = HAM1 - us10)%>%
  mutate(FundExcess_ham2 = HAM2 - us10)%>%
  mutate(FundExcess_ham3 = HAM3 - us10)%>%
  mutate(FundExcess_ham4 = HAM4 - us10)

Alpha_ham1=lm(FundExcess_ham1~MktExcess,data=data)
Alpha_ham2=lm(FundExcess_ham2~MktExcess,data=data)
Alpha_ham3=lm(FundExcess_ham3~MktExcess,data=data)
Alpha_ham4=lm(FundExcess_ham4~MktExcess,data=data)
summary(Alpha_ham1)
summary(Alpha_ham2)
summary(Alpha_ham3)
summary(Alpha_ham4)

################################################################################
data <- as.data.frame(managers) %>%
  rename(us10 = `US 10Y TR`) %>%
  rename(SP500=`SP500 TR`)


Return.cumulative(managers$HAM1,geometric = TRUE)
chart.CumReturns(managers$HAM1,wealth.index = FALSE, geometric = TRUE)

(8000*3.126671)+8000
################################################################################

T1<- TreynorRatio(managers$HAM1,managers$`SP500 TR`,managers$`US 10Y TR`)
T2<- TreynorRatio(managers$HAM2,managers$`SP500 TR`,managers$`US 10Y TR`)
T3<- TreynorRatio(managers$HAM3,managers$`SP500 TR`,managers$`US 10Y TR`)
T4<- TreynorRatio(managers$HAM4,managers$`SP500 TR`,managers$`US 10Y TR`)

T1
T2
T3
T4
################################################################################
setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
data  <- read.csv("Final_Exam_Factors.csv")
head(data)

data <- data %>%
  mutate(NVDA_RF=NVDA-RF) %>%
  mutate(INTC_RF=INTC-RF)

nvda <- lm(NVDA_RF ~ MKT_RF+SMB+HML+MOM+BAB+QMJ, data = data)
intc <- lm(INTC_RF ~ MKT_RF+SMB+HML+MOM+BAB+QMJ, data = data)

summary(nvda)
summary(intc)

################################################################################
setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
data  <- read.csv("KAG_wrangled_dataset.csv")
head(data)

data %>%
  filter(Impressions > 10000) %>%
  group_by(gender) %>%
  summarise(c = n())
  
383/(406+383)
################################################################################
setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
data  <- read.csv("KAG_wrangled_dataset.csv")
head(data)

data2 <- data %>%
  filter(xyz_campaign_id == 916) %>%
  mutate(CR = Approved_Conversion/Total_Conversion)
  
mean(data2$CR)
################################################################################
setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
data  <- read.csv("KAG_wrangled_dataset.csv")
head(data)

data2 <- data %>%
  group_by(age, gender) %>%
  summarise(total_spent = sum(Spent), total_clicks = sum(Clicks)) %>%
  mutate(avg_cost_per_click = total_spent/total_clicks)


head(data2)

################################################################################

setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
data  <- read.csv("sample_data.csv")

data <- cbind(data, xbar = rowMeans(data[,2:6]))
data <- cbind(data, min = apply (data[,2:6],1,min))
data <- cbind(data, max = apply (data[,2:6],1,max))
data <- data %>%
  mutate(range = max-min)
data
xbb = mean(data$xbar)
rbar = mean(data$range)

UCLr = 2.115*rbar
lclr = 0*rbar

UCLx= xbb+0.577*rbar
LCLx= xbb-0.577*rbar

UCLr
lclr
UCLx
LCLx

################################################################################

setwd("/users/mrajagopal/Documents/Georgiatech/Sem3/Finals")
data  <- read.csv("Store_Demand_Final.csv")
head(data)

data$date <- as.Date(as.character(data$Date), format="%m/%d/%Y")
class(data$date)
head(data)

demand_model <- ses(data$total_demand, alpha = .25, h = 5)

accuracy(demand_model)
autoplot(demand_model)

################################################################################


