#load voting data
#2019 voting

Green2019 <- read.csv("Structured Data/greenVotes2019.csv")

#remove irrelevant geographical information and electorate number 
Green2019 <- Green2019[, -c(3,4,5,6)]

#remove commas
Green2019$Green.Votes_2019 <- gsub(",", "", Green2019$Green.Votes_2019)
Green2019$Total.votes_2019 <- gsub(",", "", Green2019$Total.votes_2019)

#replace empty cells with 0
Green2019$Green.Votes_2019 <- ifelse(Green2019$Green.Votes_2019 == "", 0, Green2019$Green.Votes_2019)

#Writing green vote numbers and total vote numbers as numeric
Green2019$Green.Votes_2019 <- as.numeric(Green2019$Green.Votes_2019)
Green2019$Total.votes_2019 <- as.numeric(Green2019$Total.votes_2019)

#recalculating the vote share since they are rounded in original
Green2019$VoteShare_2019 <- Green2019$Green.Votes_2019/Green2019$Total.votes_2019*100

#compare with the percentages in the document
#they match when rounded
#remove everything except for constituency code, name, year and new calculated vote share
Green2019 <- Green2019[, -c(2,3,4,5,6)]
colnames(Green2019) <- c("id", "voteShare")
write.csv(Green2019, file = "Maps/green2019.csv", row.names = FALSE)

#2019 voting end

#2017 voting
Green2017 <- read.csv("Structured Data/greenVotes2017.csv")

Green2017 <- Green2017[, -c(3,4,5,6)]
Green2017$Green.Votes_2017 <- gsub(",", "", Green2017$Green.Votes_2017)
Green2017$Total.votes_2017 <- gsub(",", "", Green2017$Total.votes_2017)
Green2017$Green.Votes_2017 <- ifelse(Green2017$Green.Votes_2017 == "", 0, Green2017$Green.Votes_2017)
Green2017$Green.Votes_2017 <- as.numeric(Green2017$Green.Votes_2017)
Green2017$Total.votes_2017 <- as.numeric(Green2017$Total.votes_2017)
Green2017$VoteShare_2017 <- Green2017$Green.Votes_2017/Green2017$Total.votes_2017*100
Green2017 <- Green2017[, -c(2,3,4,5,6)]
colnames(Green2017) <- c("id", "voteShare")
write.csv(Green2017, file = "Maps/green2017.csv", row.names = FALSE)
#2017 voting end

#2015 voting
Green2015 <- read.csv("Structured Data/greenVotes2015.csv")


Green2015 <- Green2015[, -c(3,4,5,6)]
Green2015$Green.Votes_2015 <- gsub(",", "", Green2015$Green.Votes_2015)
Green2015$Total.votes_2015 <- gsub(",", "", Green2015$Total.votes_2015)
Green2015$Green.Votes_2015 <- ifelse(Green2015$Green.Votes_2015 == "", 0, Green2015$Green.Votes_2015)
Green2015$Green.Votes_2015 <- as.numeric(Green2015$Green.Votes_2015)
Green2015$Total.votes_2015 <- as.numeric(Green2015$Total.votes_2015)
Green2015$VoteShare_2015 <- Green2015$Green.Votes_2015/Green2015$Total.votes_2015*100
Green2015 <- Green2015[, -c(2,3,4,5,6)]
colnames(Green2015) <- c("id", "voteShare")
write.csv(Green2015, file = "Maps/green2015.csv", row.names = FALSE)
#2015 voting end

#2010 voting
Green2010 <- read.csv("Structured Data/greenVotes2010.csv")

Green2010 <- Green2010[, -c(3,4,5,6)]
Green2010$Green.Votes_2010 <- gsub(",", "", Green2010$Green.Votes_2010)
Green2010$Total.votes_2010 <- gsub(",", "", Green2010$Total.votes_2010)
Green2010$Green.Votes_2010 <- ifelse(Green2010$Green.Votes_2010 == "", 0, Green2010$Green.Votes_2010)
Green2010$Green.Votes_2010 <- as.numeric(Green2010$Green.Votes_2010)
Green2010$Total.votes_2010 <- as.numeric(Green2010$Total.votes_2010)
Green2010$VoteShare_2010 <- Green2010$Green.Votes_2010/Green2010$Total.votes_2010*100
Green2010 <- Green2010[, -c(2,3,4,5,6)]
colnames(Green2010) <- c("id", "voteShare")
write.csv(Green2010, file = "Maps/green2010.csv", row.names = FALSE)
#2010 voting end