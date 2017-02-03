
# Exercise 2 - Wrangling data

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header = TRUE)

dim(lrn14)
# The dataset comprises 183 participants and 60 variables

str(lrn14)
#Most scale-items are defined as there were measuerd with interval scales, but they appear to be ordered categorical to my mind.
#Age, attitude and points are really interval scales; gender is defined as a factor.

library(dplyr)

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30")
surface_questions <- c("SU02", "SU10", "SU18", "SU26", "SU05", "SU13", "SU21", "SU29", "SU08", "SU16", "SU24", "SU32")
strategic_questions <- c("ST01", "ST09", "ST04", "ST17", "ST25", "ST12", "ST20", "ST28")

deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

keep_columns <- c("gender", "Age", "Attitude", "deep", "surf", "stra", "Points")

learning2014 <-select(lrn14, one_of(keep_columns))

learning2014 <- filter (learning2014, Points > 0)

str(learning2014)

?write.csv()

write.csv(learning2014, "learning14.csv")

write.csv(learning2014, file = "learning14.csv", row.names = FALSE)

read.csv("learning14.csv")


 