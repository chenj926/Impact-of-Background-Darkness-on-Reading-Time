install.packages('rstatix')
library('rstatix')
zeropercent <- c(6.99, 13.56, 10.89, 12.40, 5.70, 9.98, 8.71, 18.06, 9.31, 7.79,
                 9.61, 8.26, 10.61, 10.30, 7.89, 8.49, 12.43, 7.63, 6.41, 10.48,
                 9.53, 16.73, 14.07, 6.53)
fourtypercent  <- c(7.79, 14.13, 18.37, 12.99, 4.00, 9.08, 9.23, 14.92, 10.66, 7.11,
                    10.91, 8.91, 10.83, 10.70, 7.11, 9.90, 13.05, 7.33, 6.96, 9.38,
                    8.66, 8.78, 13.35, 7.28)
eightypercent <- c(6.89, 14.55, 11.15, 12.29, 5.00, 8.09, 9.28, 10.31, 10.58, 7.36,
                   10.46, 8.25, 13.52, 10.81, 9.85, 7.22, 16.79, 7.83, 7.29, 11.97,
                   9.28, 10.95, 10.95, 7.70)

mydata = data.frame(
  Reading_Time = c(zeropercent, fourtypercent, eightypercent),
  Backgrounddarkness = factor(rep(c("0%", "40%", "80%"), rep(24, 3))),                        readers = factor(rep(c(1:24),3)))
res.aov<-aov(Reading_Time ~ Backgrounddarkness+readers, data = mydata)
summary(res.aov)

shapiro.test(zeropercent)

shapiro.test(fourtypercent)

shapiro.test(eightypercent)

par(mfcol = c(1,3))

# Histogram for 0% Data
hist(zeropercent, main = "Histogram for 0% Data", xlab = "Reading Time", ylab = "Frequency", cex.lab = 1.5, cex.main = 1.5)

# Histogram for 40% Data
hist(fourtypercent, main = "Histogram for 40% Data", xlab = "Reading Time", ylab = "Frequency", cex.lab = 1.5, cex.main = 1.5)

# Histogram for 80% Data
hist(eightypercent, main = "Histogram for 80% Data", xlab = "Reading Time", ylab = "Frequency", cex.lab = 1.5, cex.main = 1.5)


par(mfcol = c(1,3))
qqnorm(zeropercent, main = "QQ Plot for Zero Percent Data", cex = 2)
qqline(zeropercent, cex = 2)
qqnorm(fourtypercent, main = "QQ Plot for Fourty Percent Data", cex = 2)
qqline(fourtypercent, cex = 2)
qqnorm(eightypercent, main = "QQ Plot for Eighty Percent Data", cex = 2)
qqline(eightypercent, cex = 2)

bartlett.test(Reading_Time ~ Backgrounddarkness, data = mydata)



# Perform repeated measures ANOVA using anova_test()
anova_result <- anova_test(
  data = mydata,    # Your data frame
  dv = Reading_Time,    # Dependent variable
  wid = readers,    # Variable identifying the case/sample
  within = Backgrounddarkness    # Within-subjects factor
)

# Print the ANOVA table
print(anova_result)

summary(zeropercent)
summary(fourtypercent)
summary(eightypercent)

sd(zeropercent)
sd(fourtypercent)
sd(eightypercent)

fourtypercent <- (fourtypercent - mean(fourtypercent))/sd(fourtypercent)
eightypercent <- (eightypercent - mean(eightypercent))/sd(eightypercent)
zeropercent <- (zeropercent - mean(zeropercent))/sd(zeropercent)

mydata = data.frame(
  Reading_Time = c(zeropercent, fourtypercent, eightypercent),
  Backgrounddarkness = factor(rep(c("0%", "40%", "80%"), rep(24, 3))),                        readers = factor(rep(c(1:24),3)))

boxplot(Reading_Time ~ Backgrounddarkness, data = mydata, ylim = c(-3,3), xlab =
          "Backgrounddarkness", ylab = "Reading_Time")

abline(h = 3, col = "red")
abline(h = -3, col = "red")

