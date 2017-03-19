
# ---------- HUMAN RESOURCES ANALYSIS -------------

library(ggplot2)
library(corrplot)

# import dataset from kaggle
df <- read.csv(file.choose())

# check out the dataframe
head(df)
summary(df)
str(df)

#-------------- Exploration & Visualization

# Re-factor variables
df$sales<-as.factor(df$sales)
df$salary<-as.factor(df$salary)
df$salary<-ordered(df$salary,levels=c("low","medium","high"))

# salary by department
sal <- ggplot(data=df, aes(x=sales))
sal + geom_histogram(stat="count", aes(fill=salary), 
                     binwidth=10, 
                     color="Black") +
  xlab("Department") +
  ylab("Count") +
  ggtitle("Salary Distribution by Department") +
  scale_fill_discrete(name="Salary")

# avg monthly hours vs. satisfaction
t <- ggplot(data=df, aes(x=average_montly_hours, y=satisfaction_level))
t + geom_point(aes(color=time_spend_company, size=factor(left))) +
  geom_smooth() +
  xlab("Avg. Hours/Month") + 
  ylab("Level of Satisfaction") + 
  ggtitle("Satisfaction level by Avg. monthly hours")

# satisfaction in relation to salary
a <- ggplot(data=df, aes(x=salary, y=satisfaction_level, 
                         fill=factor(left)))
a + geom_boxplot() + 
  xlab("Salary") + 
  ylab("Level of Satisfaction") +
  ggtitle("Satisfaction by Salary") +
  scale_fill_discrete(name="Left Company")

# time spend in company versus monthly hours 
c <- ggplot(data=df, aes(x=factor(time_spend_company), 
                         y=average_montly_hours, 
                         fill=factor(left)))
c + geom_boxplot() +
  xlab("Years at company") + 
  ylab("Avg. Hours/Month") + 
  ggtitle("Hours per Month by Company years") +
  scale_fill_discrete(name="Left Company")


# correlation analysis
cor(df[,1:8])
corrplot(cor(df[,1:8]), method="shade")
