read.csv("learning14.csv")

learning2014

str(learning2014)
mean(learning2014$Age)

str(learning2014$gender)

table(learning2014$gender)

library(ggplot2)

#I have created a couple of graphs with ggplot2. The first one has appeared to be interesting,
# but only at the first sight (I tested the combined effect of deep and gender on points, and
# it was not different from zero).

plot1a <- ggplot(learning2014, aes(x=deep, y=Points, col = gender))
plot1b <- plot1a + geom_smooth(method = "lm")             
plot1b          

plot2a <- ggplot(learning2014, aes(x=surf, y=Points, col = gender))
plot2b <- plot2a + geom_smooth(method = "lm")             
plot2b          

plot3a <- ggplot(learning2014, aes(x=stra, y=Points, col = gender))
plot3b <- plot3a + geom_smooth(method = "lm")             
plot3b          

head(learning2014)

# Also, I have performed a set of bivariate analyses to test the individual effects of
# age, gender and learning attitude on the outcome variable.

cor.test(learning2014$Age, learning2014$Points)
cor.test(learning2014$Attitude, learning2014$Points)
t.test(learning2014$Points ~ learning2014$gender)

#I performed at least a dozen regressions with IVs in different combinations and included
# interaction terms, but as a matter of fact, none of them was really interesting. This is pity
# because I thought that attitudes and learning styles would interact in predicting points... So I kept
# the following model, where attitude is defined as IV and the model is controlled for gender and age.

model <- lm(learning2014$Points ~ learning2014$Attitude + learning2014$gender + learning2014$Age)
summary(model)

# The model was significant, and explained 19 % of the variance in the DV. Positive learning attitudes
# contributed to a better efficiancy in points. Age and gender had no significant effects.

model <- lm(learning2014$Points ~ learning2014$Attitude + learning2014$surf + learning2014$stra + learning2014$deep)
summary(model)

#I added the three learning styles to the model; none of them was significant. therefore I ran a final model, which include
#only attitude as explanatory variable. The model was significant, and explained 19% of the variance
#in the dependent variable, F(1, 164) = 38.61, p <.001. The unsrandardized regression coefficients of attitude was B=.35.
# This can be interpreted as more positive attitudes toward learning resultaed in higher points.

final_model <- lm(learning2014$Points ~ learning2014$Attitude)
summary(final_model)

#I created plots to diagnostize model fit. Theere seems to be some violaationn against normality in each plot.
par(mfrow = c(2,2))
plot(final_model, which = c(1, 2, 5))


