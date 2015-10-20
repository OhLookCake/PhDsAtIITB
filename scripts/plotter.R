
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gtable)
library(scales)

df <- read.csv('../data/formatteddata_clean.csv', header=T)
dfPrograms <- read.csv('../data/programsdata.csv',  header=T)



#### C. Insights & Visualization ####

trimmedMean <- function(x, frac = 0.1){
  mean(x, trim = frac)
}


ggplot(data = dfPrograms, aes(
                            x = Startedin,
                            y = reorder(Department, Startedin),
                            color = Department,
                            group = Department)) + geom_point() + ylab("Department") + theme(legend.position="none")

coeffs <- coef(lm(FinishYear ~ StartYear, data = df))

ggplot(data = df, aes(
                      x = StartYear,
                      y = FinishYear)) + geom_point(size=6,alpha=0.04, colour="#112288") +
  coord_fixed() + geom_abline(intercept = coeffs[1], slope = coeffs[2], colour = "#EE2211", size = 2)

#By Department

p <- ggplot(data=df, aes(
                      x = Length,
                      y = reorder(Dept, Length, FUN = trimmedMean),
                      color = Dept,
                      group = Dept)) 

p2 <- p + geom_point(size=4,alpha=0.3, position = position_jitter(height = 0.3)) +
          geom_errorbarh(stat = "vline", xintercept = "trimmedMean", height=0.6, size=1,
                         aes(xmax=..x..,xmin=..x..), color="black")

p3 <- p2 +
        xlab("Ph.D Length") + ylab("Department") + 
        scale_x_continuous(breaks=1:15, limits = c(1,15))
        
        
p3 +  theme(axis.text.y = element_text(size = 16, face="bold"), axis.text.x = element_text(size = 14),
            axis.title = element_text(size=14,face="bold"), axis.title.x = element_text(vjust= -0.5)) +
      theme(legend.position="none") +
      theme(panel.grid.minor = element_blank())


#Global Hist

ggplot(data = df, aes(x = factor(Length), fill = 1))  + geom_histogram(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = percent_format(), limits = c(0, .35), expand = c(.01,0)) + 
  xlab("Ph.D Length in years") + ylab("Percent of Students") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
        legend.position="none") + 
  theme(axis.text.y = element_text(size = 14, face="bold"), axis.text.x = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"),
        axis.title.x = element_text(vjust= -0.5), axis.title.y = element_text(vjust= 1.5)) 


ggplot(data = df, aes(x = Length, fill=Dept))  + geom_bar(binwidth = 1, position="fill")
ggplot(data = df, aes(x = Length, colour = Dept))  + geom_freqpoly(binwidth=1)

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}

ggplot(data = df, aes(x = StartYear,
                      y = Length)) + geom_point(size = 4, alpha = 0.4) + stat_sum_single(mean, geom="line") + 
  theme_bw() + theme(panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

coef(lm(Length ~ StartYear, data = df))
cor(df$Length, df$StartYear)
