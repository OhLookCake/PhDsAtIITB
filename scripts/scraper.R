library(XML)
library(plyr)

#### A. Data Gathering ####
#TODO: generate this via code. Basically, just put the grep into R.
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



l.dfTime.rawtables <- lapply(pages, readHTMLTable, header=T, which=1,stringsAsFactors=F)
l.dfTime.rawtables <- lapply(1:length(pages), function(i) {l.dfTime.rawtables[[i]]$PageString = pages[i]; l.dfTime.rawtables[[i]]})





#### B. Data Cleaning ####

dfTime.longtable <- do.call(rbind, l.dfTime.rawtables)
stopifnot( sum(sapply(l.dfTime.rawtables, nrow)) == nrow(dfTime.longtable) ) # assert to verify correct concatenation

dfTime.longtable$FinishYear <- as.numeric(dfTime.longtable$SubmissionYear)
dfTime.longtable$StartYear <- 1900 + as.numeric(substr(dfTime.longtable$RollNo, 1, 2)) # Extracting join year from roll number
dfTime.longtable$StartYear[dfTime.longtable$StartYear < 1950] <- dfTime.longtable$StartYear[dfTime.longtable$StartYear < 1950] + 100 #Fixing those from 2000 onwards

dfTime <- dfTime.longtable[, c("StartYear", "FinishYear")]
dfTime$Department <- ldply(strsplit(dfTime.longtable$PageString, "[\\&\\=]"))$V2 # Department from the url
dfTime$Department <- gsub("\\+", " ", dfTime$Department)
dfTime$Length <- dfTime$FinishYear - dfTime$StartYear

dfTime$Department <- gsub("%26", "&", dfTime$Department)

write.csv(dfTime, "../data/formatteddata.csv", quote=F, row.names=F)

# B2. Discarding errors (Verified that these are errors)

dfTime <- dfTime[dfTime$Length < 15, ]
dfTime$FinishYear[dfTime$Length < 0] <- 2013
dfTime$Length <- dfTime$FinishYear - dfTime$StartYear

#CTARA being dropped because it's so new that there's only one student who has completed his PhD
dfTime <- dfTime[dfTime$Department != "CTARA", ]

# B3. Merges (Unconfirmed!)
dfTime$Department <- gsub("^Materials Science$", "Metallurgical Engineering and Materials Science", dfTime$Department)
dfTime$Department <- gsub("^School of Information Technology$", "Computer Science and Engineering", dfTime$Department)
dfTime$Department <- gsub("^Bio-Medical Engineering$", "Biosciences & Bioengineering", dfTime$Department)
dfTime$Department <- gsub("^Bio-Technology$", "Biosciences & Bioengineering", dfTime$Department)
dfTime$Department <- gsub("^Industrial Management$", "School of Management", dfTime$Department)

# B4. Shorter names
dfTime$Dept <- dfTime$Department
dfTime$Dept <- gsub("^Aerospace Engineering$", "Aero", dfTime$Dept)
dfTime$Dept <- gsub("^Biosciences & Bioengineering$", "Bio", dfTime$Dept)
dfTime$Dept <- gsub("^Centre for Research in Nano Technology and Sciences$", "NanoTech", dfTime$Dept)
dfTime$Dept <- gsub("^Centre of Studies in Resources Engineering$", "CSRE", dfTime$Dept)
dfTime$Dept <- gsub("^Chemical Engineering$", "Chemical", dfTime$Dept)
dfTime$Dept <- gsub("^Civil Engineering$", "Civil", dfTime$Dept)
dfTime$Dept <- gsub("^Computer Science and Engineering$", "CSE", dfTime$Dept)
dfTime$Dept <- gsub("^Corrosion Science and Engineering$", "Corrosion", dfTime$Dept)
dfTime$Dept <- gsub("^Electrical Engineering$", "Elec", dfTime$Dept)
dfTime$Dept <- gsub("^Energy Systems Engineering$", "Energy", dfTime$Dept)
dfTime$Dept <- gsub("^Environmental Science and Engineering$", "Enviro", dfTime$Dept)
dfTime$Dept <- gsub("^Humanities and Social Science$", "HSS", dfTime$Dept)
dfTime$Dept <- gsub("^Industrial Design Centre$", "IDC", dfTime$Dept)
dfTime$Dept <- gsub("^Industrial Engineering and Operations Research$", "IEOR", dfTime$Dept)
dfTime$Dept <- gsub("^Mathematics$", "Math", dfTime$Dept)
dfTime$Dept <- gsub("^Mechanical Engineering$", "Mech", dfTime$Dept)
dfTime$Dept <- gsub("^Metallurgical Engineering and Materials Science$", "Meta", dfTime$Dept)
dfTime$Dept <- gsub("^Reliability Engineering$", "Reliability", dfTime$Dept)
dfTime$Dept <- gsub("^School of Management$", "SOM", dfTime$Dept)
dfTime$Dept <- gsub("^Systems and Control Engineering$", "SysCon", dfTime$Dept)





# B5. Write to disk

write.csv(dfTime, "../data/formatteddata_clean.csv", quote=F, row.names=F)


dfFirstPhdJoins <- ddply(dfTime,.variables = "Department", summarize, Startedin = min(StartYear))
dfLastPhdDegree <- ddply(dfTime,.variables = "Department", summarize, LastDegree = max(FinishYear))

dfPrograms <- merge(dfFirstPhdJoins, dfLastPhdDegree)
write.csv(dfPrograms, "../data/programsdata.csv", quote=F, row.names=F)



