
#First, I read the data from the webpage to R.

library(data.table)

data <- fread('http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt')
str(data)
dim(data)
glimpse(data)

#I chose the following four explanatory variables. The first
#sex        student's sex (binary: 'F' - female or 'M' - male) 
#studytime  weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours) 
#health     current health status (numeric: from 1 - very bad to 5 - very good) 
#goout  going out with friends (numeric: from 1 - very low to 5 - very high) 

#The four hypos are as follows: 
#(H1) Males are more often heavy drnkers than females.
#(H2) People spending less time with their studies are more often heavy drinker than the rest.
#(H3) The worse the healt situation, the greater the probability that somebody is a hevay drinker.
#(H4) The more often somebody goes out with his/her friends, the greater the probability that somebody is a hevay drinker.


#I checked the bivariate relationships between the dependent variable and the four explanatory variables
#by the means of plotting and bivariate analyses.
#I read ggplot2.

library(ggplot2)

ggplot(data,aes(x=studytime, fill=high_use)) + geom_bar(position="dodge")
ggplot(data,aes(x=health, fill=high_use)) + geom_bar(position="dodge")
ggplot(data,aes(x=goout, fill=high_use)) + geom_bar(position="dodge")

t.test(data$studytime ~ data$high_use)
t.test(data$health ~ data$high_use)
t.test(data$goout ~ data$high_use)

#Taken together, both the plotting and the t-test indicate that there is no difference in perceived healt
#between the two group, but the difference is remarkable with respect the two other variables. The less somebody
#spends with studying and the more often somebody goes out with friends increase the probability of the person
#is a heavy drinker

#Now I check the role of gender in drinking habits.
tbl = table(data$high_use, data$sex)
prop.table(tbl, 2)
chisq.test(tbl)

#The crosstabulation and the related chi-square analysis indicated that male students are more often
#heavy drinkers than female mstudents.

#Here comes the logistic regression. I open also the rcompanion library to get results about model fit.
library(rcompanion)

log_reg <- glm(high_use ~ sex + health + goout + studytime, data = data, family = "binomial")
summary(log_reg)
nagelkerke(log_reg)
exp(cbind(OR = coef(log_reg), confint(log_reg)))

#The model was significant, x2(4) =72.97, p <.001, and accodring to Nagelkerke pseudo R-square
# it explained 25% of the variation in high alcohol consumption. Out of the four explanatory variables
#perceived health state had no significant effect, however, the other three variables explained signifi-
#cantly the dependent variable. Specifically, it was shown that the relative probability that a male is
# heavy drinker is about 1.89 times greater than that a female becomes heavy drinker. The interpretation of the
# the two other explanatory variables is more difficult; basically, increase in goout and decrease in studytime
# results in an increase in the probability that somebody is a heavy drinker (it would have perhaps been better
#to use these variables as ordered categorical, but explaining the efects would have been difficult even so)"


#I created the classification table according to instructions on the R homepage. According to these results,
#76.4% of the cases could be correctly classified by the model.

Yhat <- fitted(log_reg)
thresh <- 0.5
YhatFac <- cut(Yhat, breaks=c(-Inf, thresh, Inf), labels=c("low", "high"))

# contingency table and marginal sums
Class_tab <- table(data$high_use, YhatFac)
addmargins(Class_tab)
sum(diag(Class_tab)) / sum(Class_tab)


#I could not draw the plot as the lecture disappeared from Datacamp and I could not follow the instructions.
# Yet, I reveal with the following logistic regression that the effect of going out with friends differs between the two gender.


log_reg2 <- glm(high_use ~ sex*goout + health + goout + studytime, data = data, family = "binomial")
summary(log_reg2)
nagelkerke(log_reg2)
exp(cbind(OR = coef(log_reg2), confint(log_reg2)))

#Here comes the chi_square difference testing between the two logit models.
# The change in chi-square is 10.65, which at one degree of freedom is statistically significant at level p <.001;
#that said, the model with the interaction term fits better the data, also Nagelkerke explains 28% in the variation
# of the DV, that is, 3% more than in the model without interaction.
anova(log_reg, log_reg2, test = "Chisq") 

#Here comes the plot visuailizing the interaction.

PredProb<- predict(log_reg2, type="response")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ggplot(data, aes(x=goout, y=PredProb, col=sex)) + 
  geom_jitter(height = 0.05) +
  binomial_smooth()






