library(XML)
library(plyr)
library(ggplot2)

#### A. Data Gathering ####

pages <- c("http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Aerospace+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Bio-Medical+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Bio-Technology&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Biosciences+%26+Bioengineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Centre+for+Research+in+Nano+Technology+and+Sciences&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Centre+of+Studies+in+Resources+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Chemical+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Chemistry&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Civil+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Computer+Science+and+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Corrosion+Science+and+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=CTARA&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Earth+Science&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Electrical+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Energy+Systems+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Environmental+Science+and+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Humanities+and+Social+Science&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Industrial+Design+Centre&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Industrial+Engineering+and+Operations+Research&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Industrial+Management&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Materials+Science&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Mathematics&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Mechanical+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Metallurgical+Engineering+and+Materials+Science&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Physics&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Reliability+Engineering&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=School+of+Information+Technology&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=School+of+Management&prog=Ph.D",
           "http://etd.library.iitb.ac.in/etd/Etd_Query.jsp?dept=Systems+and+Control+Engineering&prog=Ph.D")



l.df.rawtables <- lapply(pages, readHTMLTable, header=T, which=1,stringsAsFactors=F)
l.df.rawtables <- lapply(1:length(pages), function(i) {l.df.rawtables[[i]]$PageString = pages[i]; l.df.rawtables[[i]]})





#### B. Data Cleaning ####

df.longtable <- do.call(rbind, l.df.rawtables)
stopifnot( sum(sapply(l.df.rawtables, nrow)) == nrow(df.longtable) ) # assert to verify correct concatenation

df.longtable$FinishYear <- as.numeric(df.longtable$SubmissionYear)
df.longtable$StartYear <- 1900 + as.numeric(substr(df.longtable$RollNo, 1, 2)) # Extracting join year from roll number
df.longtable$StartYear[df.longtable$StartYear < 1950] <- df.longtable$StartYear[df.longtable$StartYear < 1950] + 100 #Fixing those from 2000 onwards

df <- df.longtable[, c("StartYear", "FinishYear")]
df$Department <- ldply(strsplit(df.longtable$PageString, "[\\&\\=]"))$V2 # Department from the url
df$Department <- gsub("\\+", " ", df$Department)
df$Length <- df$FinishYear - df$StartYear


write.csv(df, "../data/formatteddata.csv", quote=F, row.names=F)

# B2. Discarding errors (Verified that these are errors)
sum(df$Length < 0)
df <- df[df$Length > 0, ]
sum(df$Length >15)
df <- df[df$Length < 15, ]


# B3. Merges (Unconfirmed!)





# B4. Write to disk

write.csv(df, "../data/formatteddata_clean.csv", quote=F, row.names=F)


dfFirstPhdJoins <- ddply(df,.variables = "Department", summarize, Startedin = min(StartYear))
dfLastPhdDegree <- ddply(df,.variables = "Department", summarize, LastDegree = max(FinishYear))

dfPrograms <- merge(dfFirstPhdJoins, dfLastPhdDegree)
write.csv(dfPrograms, "../data/programsdata.csv", quote=F, row.names=F)




#### C. Insights & Visualization ####

p <- ggplot(data=df, aes(
                      x = Length,
                      y = reorder(Department, Length),
                      color = Department,
                      group = Department)) 


p + geom_point(size=4,alpha=0.4, position = position_jitter(height = 0.3)) +
  geom_errorbarh(stat = "vline", xintercept = "median",
                 height=0.6, size=1,
                 aes(xmax=..x..,xmin=..x..),color="black") +
  geom_errorbarh(stat = "vline", xintercept = "mean",
                 height=0.6, size=1,
                 aes(xmax=..x..,xmin=..x..),color="red") +
  theme(legend.position="none") +
  xlab("Ph.D Length") + ylab("Department")



ggplot(data = dfPrograms, aes(
                  x = Startedin,
                  y = reorder(Department, Startedin),
                  color = Department,
                  group = Department)) + geom_point()

ggplot(data = df, aes(
                  x = Length,
                  y = factor(StartYear),
                  group = StartYear)) + geom_point(size=4,alpha=0.2, position = position_jitter(height = 0.2))




