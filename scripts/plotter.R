
library(ggplot2)

df <- read.csv('../data/formatteddata_clean.csv', header=T)
dfPrograms <- read.csv('../data/programsdata.csv',  header=T)



#### C. Insights & Visualization ####

p <- ggplot(data=df, aes(
  x = Length,
  y = reorder(Department, Length),
  color = Department,
  group = Department)) 


p + geom_point(size=4,alpha=0.4, position = position_jitter(height = 0.3)) +
  geom_errorbarh(stat = "vline", xintercept = "mean", height=0.6, size=1,
                 aes(xmax=..x..,xmin=..x..),color="black") +
  theme(legend.position="none") +
  xlab("Ph.D Length") + ylab("Department")


ggplot(data = dfPrograms, aes(
  x = Startedin,
  y = reorder(Department, Startedin),
  color = Department,
  group = Department)) + geom_point() + ylab("Department")

ggplot(data = df, aes(
  x = StartYear,
  y = FinishYear)) + geom_point(size=4,alpha=0.03)


