###### Tips Analysis #######

#### Libraries
library(tidyverse)
library(chron)

#### Loading in data
tips <- read.csv("tips.csv", as.is = T, header = T)
tips

#### Fixing structure
tips$DATE <- as.Date(tips$DATE, "%Y-%m-%d")
tips$STATE <- as.factor(tips$STATE)
tips$TIME_START <- as.POSIXct(tips$TIME_START, format="%H:%M")
tips$TIME_END <- as.POSIXct(tips$TIME_END, format="%H:%M")

str(tips)

#### Feature Engineering
tips$FINAL_TIP <- tips$BEFORE_TIPOUT - tips$AFTER_TIPOUT
tips$SHIFT_LENGTH <- tips$TIME_END - tips$TIME_START

#### Looking at data

### First just looking at the general tip vs. state plots

# First Boxplot (Before Tip Out)
tips %>% ggplot(aes(x = STATE, y = BEFORE_TIPOUT)) + 
  geom_boxplot(color = "Blue") + 
  theme_bw() + 
  ggtitle("Overview of Make Up v. No Make Up \n - Before Tip Out - ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Make Up Situation") + 
  ylab("Amount (Before Tip Out)")

# Second Boxplot (After Tip Out)
tips %>% ggplot(aes(x = STATE, y = AFTER_TIPOUT)) + 
  geom_boxplot(color = "Blue") + 
  theme_bw() + 
  ggtitle("Overview of Make Up v. No Make Up \n - After Tip Out - ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Make Up Situation") + 
  ylab("Amount (After Tip Out)")

# Third Boxplot (Overall Pay)
tips %>% ggplot(aes(x = STATE, y = FINAL_TIP)) + 
  geom_boxplot(color = "Blue") + 
  theme_bw() + 
  ggtitle("Overview of Make Up v. No Make Up \n - Overall Tip - ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Make Up Situation") + 
  ylab("Amount (Overall Tip)")


### Now, looking at whether or not the length of the shift effected the pay
tips %>% ggplot(aes(x = as.numeric(SHIFT_LENGTH), y = FINAL_TIP, color = STATE)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  theme_bw(base_size = 12, base_family = "Times New Roman") + 
  ggtitle("Overview of Overall Tip v. Shift Length - ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Shift Length") + 
  ylab("Amount (Overall Tip)") +
  scale_color_manual(values = c("Red", "Blue"))

# Averages: Make Up v. No Make Up v. Shift Length
tips %>% ggplot(aes(x = SHIFT_LENGTH, y = FINAL_TIP, color = STATE)) + 
  geom_violin() + 
  theme_bw() + 
  ggtitle("Spread of Tips Across Shift Lengths") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Shift Length") + 
  ylab("Amount (Overall Tip)") +
  scale_color_manual(values = c("Red", "Blue"))


##### Statistical Analysis

### The simple approach is to just use a t-test and see
### whether there is an effect here:

## There seems to be no significant difference
anova(lm(FINAL_TIP ~ STATE, data = tips))

## Considering residuals
fit_1 <- lm(FINAL_TIP ~ STATE, data = tips)
plot(fit_1)
summary(fit_1)
(-0.476)^2

ggplot(data = tips, aes(x = fit_1$fitted.values, y = fit_1$residuals)) + 
  theme_bw() + geom_point() + 
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residual Plot: ANOVA Model") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept = 0)

## Let's consider some other effects which may be occuring 
qqnorm(fit_1$residuals, pch = 1, frame = FALSE)
qqline(fit_1$residuals, col = "steelblue", lwd = 2)
head(tips)

## Question: Does overall sales increase with make up vs. no make up?
# Fourth Boxplot (Overall Pay)
tips %>% ggplot(aes(x = STATE, y = NET_SALES)) + 
  geom_boxplot(color = "Blue") + 
  theme_bw() + 
  ggtitle("Overview of Make Up v. No Make Up \n - Overall Tip - ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Make Up Situation") + 
  ylab("Amount (Overall Tip)")

## No significant difference here either

## Okay, I guess the question is then, does there seem to be more outliers with respect
## to Make - Up v. No Make Up
## In other words, we want to look at the extreme values and whether their is more
## of them at the edges of the make up data set
## As we all know, the effect here is amplified by the fact that
## 

### Splitting data
tips_makeup <- tips %>% 
  filter(STATE == "MAKEUP")

tips_noMakeup <- setdiff(tips, tips_makeup)

makeup_money <- c()
no_makeup_money <- c()
for (i in 1:1000) {
  makeup_money[i] <- mean(sample(tips_makeup$FINAL_TIP, 11, replace = T))
  no_makeup_money[i] <- mean(sample(tips_noMakeup$FINAL_TIP, 11, replace = T))
}

hist(makeup_money)
hist(no_makeup_money)

hist(makeup_money, col="red", main = "Bootstrap Histograms", xlab = "Mean Tip Amount")
hist(no_makeup_money, add=T, col=rgb(0, 1, 0, 0.5) )

#### Seems to be some difference here when repeated using a bootstrap, the
#### average definitely deviates - the spread is greater as well for the make up
#### hisogram indicating some sort of potential relationship between make up use and tip amount

##### Okay, what else can we analyze here?
##### 
tips

# Okay, well the hair categories are tough to guage
# We can leave that for another time

# Now, time to consider whether specific shift lengths
# specifically with make up, resulted in tips similar to 
# that of shift lengths that were longer without make up

tips_makeup_short <- tips_makeup %>% 
  filter(SHIFT_LENGTH < 5.5)

tips_noMakeup_long <- tips_noMakeup %>% 
  filter(SHIFT_LENGTH > 5.5)

# First, let's check if length of shift affects how much u make
fit_2 <- lm(FINAL_TIP ~ SHIFT_LENGTH, data = tips)
summary(fit_2)

## Some significant effect
mean(tips_makeup_short$FINAL_TIP[-4])
mean(tips_noMakeup_long$FINAL_TIP)

## In fact, it seems that on shorter shifts, with this data
## with make up, there tends to be an overall greater amount of tips
## garnered

## Looking at plots of how much money made for tips vs. No tips
## We see indeed that there seems to be much steeper effect 
## for no make up vs. make up

## And, as shift length increases, it seems that this effect is even more pronounced
## which makes sense because you get exposed more to dudes who would want to check
## you out and are more likely willing to pay

## Also, it seems most shifts started earlier in the day and don't know if tips
## increased at a particular time of the day or what

fit_3 <- lm(FINAL_TIP ~ SHIFT_LENGTH, data = tips_makeup)
fit_4 <- lm(FINAL_TIP ~ SHIFT_LENGTH, data = tips_noMakeup)

summary(fit_3)
summary(fit_4)


##### So, while the traditional T-Test seems to indicate that there is 
##### no real differecne, there are a number of reasons to think that 
##### make up indeed seems to effect the results

##### Also, we need to consider what day it was - future consideration

head(tips[,c(2, 3, 4, 10, 11, 12)])
tips

names(tips)

mean(tips_makeup$FINAL_TIP[-10])/mean(as.numeric(tips_makeup$SHIFT_LENGTH))
mean(tips_noMakeup$FINAL_TIP)/mean(as.numeric(tips_noMakeup$SHIFT_LENGTH))

mean(tips_makeup$BEFORE_TIPOUT[-10])
mean(tips_noMakeup$BEFORE_TIPOUT)

sd(tips_makeup$FINAL_TIP[-10])
sd(tips_noMakeup$BEFORE_TIPOUT)
