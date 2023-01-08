library(readxl)
author_analysis <- read_excel("Desktop/thesis/author analysis.xlsx", sheet = "Sheet2", na = "NA")

author_analysis[6,"Year"] <- 2009
author_analysis[6,"Citations"] <- 13
author_analysis[6,"Time"] <- 3

author_analysis[14,"Year"] <- 2013
author_analysis[14,"Citations"] <- 252
author_analysis[14,"Time"] <- 4

author_analysis[30, "Year"] <- 2012
author_analysis[30, "Citations"] <- 41
author_analysis[30, "Time"] <- 4

author_analysis[38, "Citations"] <- 6
author_analysis[38, "Year"] <- 2018
author_analysis[38, "Time"] <- 4

author_analysis[40, "Citations"] <- 17
author_analysis[40, "Year"] <- 2014
author_analysis[40, "Time"] <- 4

author_analysis[52, "Citations"] <- 177
author_analysis[52, "Year"] <- 2015
author_analysis[52, "Time"] <- 4

author_analysis[56, "Citations"] <- 7
author_analysis[56, "Year"] <- 2017
author_analysis[56, "Time"] <- 4

author_analysis[57, "Citations"] <- 32
author_analysis[57, "Year"] <- 2016
author_analysis[57, "Time"] <- 4

author_analysis[61, "Citations"] <- 108
author_analysis[61, "Year"] <- 1996
author_analysis[61, "Time"] <- 2

author_analysis[77, "Citations"] <- 46
author_analysis[77, "Year"] <- 2004
author_analysis[77, "Time"] <- 3

author_analysis[91, "Citations"] <- 18
author_analysis[91, "Year"] <- 2019
author_analysis[91, "Time"] <- 4

author_analysis[107, "Citations"] <- 0
author_analysis[107, "Year"] <- 2022
author_analysis[107, "Time"] <- 5

author_analysis[111, "Citations"] <- 1
author_analysis[111, "Year"] <- 2021
author_analysis[111, "Time"] <- 5

author_analysis[121, "Citations"] <- 26
author_analysis[121, "Year"] <- 2020
author_analysis[121, "Time"] <- 5

author_analysis[123, "Citations"] <- 11
author_analysis[123, "Year"] <- 2019
author_analysis[123, "Time"] <- 4

author_analysis[124, "Citations"] <- 7
author_analysis[124, "Year"] <- 2018
author_analysis[124, "Time"] <- 4

author_analysis[152, "Citations"] <- 85
author_analysis[152, "Year"] <- 2000
author_analysis[152, "Time"] <- 3

author_analysis[153, "Citations"] <- 190
author_analysis[153, "Year"] <- 2013
author_analysis[153, "Time"] <- 4

author_analysis[158, "Year"] <- 2000
author_analysis[158, "Citations"] <- 200
author_analysis[158, "Time"] <- 3

author_analysis[160, "Year"] <- 2006
author_analysis[160, "Citations"] <- 91
author_analysis[160, "Time"] <- 3

author_analysis[163, "Year"] <- 2018
author_analysis[163, "Citations"] <- 28
author_analysis[163, "Time"] <- 4

author_analysis[168, "Year"] <- 2022
author_analysis[168, "Citations"] <- 0
author_analysis[168, "Time"] <- 5

author_analysis[172, "Year"] <- 2007
author_analysis[172, "Time"] <- 3
author_analysis[172, "Citations"] <- 137

author_analysis[178, "Citations"] <- 77
author_analysis[178, "Year"] <- 2012
author_analysis[178, "Time"] <- 4

author_analysis[194, "Citations"] <- 39
author_analysis[194, "Year"] <- 2015
author_analysis[194, "Time"] <- 4

author_analysis[197, "Citations"] <- 26
author_analysis[197, "Year"] <- 2005
author_analysis[197, "Time"] <- 3

author_analysis[203, "Citations"] <- 49
author_analysis[203, "Year"] <- 2013
author_analysis[203, "Time"] <- 4

author_analysis[213, "Citations"] <- 149
author_analysis[213, "Year"] <- 1976
author_analysis[213, "Time"] <- 1

author_analysis[237, "Citations"] <- 15
author_analysis[237, "Year"] <- 2018
author_analysis[237, "Time"] <- 4

author_analysis[243, "Citations"] <- 38
author_analysis[243, "Year"] <- 2017
author_analysis[243, "Time"] <- 4

author_analysis[260, "Year"] <- 2021
author_analysis[260, "Citations"] <- 0
author_analysis[260, "Time"] <- 5

author_analysis[263, "Citations"] <- 249
author_analysis[263, "Year"] <- 2007
author_analysis[263, "Time"] <- 3

author_analysis[268, "Citations"] <- 5
author_analysis[268, "Year"] <- 2016
author_analysis[268, "Time"] <- 4

author_analysis[270, "Year"] <- 2016
author_analysis[270, "Citations"] <- 69
author_analysis[270, "Time"] <- 4

author_analysis[279, "Citations"] <- 136
author_analysis[279, "Year"] <- 2015
author_analysis[279, "Time"] <- 4

author_analysis[280, "Citations"] <- 75
author_analysis[280, "Year"] <- 2015
author_analysis[280, "Time"] <- 4

author_analysis[305, "Citations"] <- 5
author_analysis[305, "Year"] <- 2013
author_analysis[305, "Time"] <- 4
author_analysis[305, "Journal"] <- "Decision Support Systems"

author_analysis[311, "Citations"] <- 104
author_analysis[311, "Year"] <- 2017
author_analysis[311, "Time"] <- 4
author_analysis[311, "Journal"] <- "Computers in Human Behavior"

author_analysis[316, "Citations"] <- 73
author_analysis[316, "Year"] <- 1991
author_analysis[316, "Time"] <- 2
author_analysis[316, "Journal"] <- "The Journal of Product Innovation Management"

author_analysis[334, "Citations"] <- 0
author_analysis[334, "Year"] <- 2021
author_analysis[334, "Time"] <- 5
author_analysis[334, "Journal"] <- "Journal of the Academy of Marketing Science"

author_analysis[337, "Year"] <- 2011
author_analysis[337, "Citations"] <- 52
author_analysis[337, "Time"] <- 4
author_analysis[337, "Journal"] <- "Journal of the Academy of Marketing Science"

author_analysis[347, "Year"] <- 2009
author_analysis[347, "Citations"] <- 15
author_analysis[347, "Time"] <- 3
author_analysis[347, "Journal"] <- "Marketing Letters"

author_analysis[350, "Citations"] <- 10
author_analysis[350, "Year"] <- 2020
author_analysis[350, "Time"] <- 5

author_analysis[361, "Citations"] <- 0
author_analysis[361, "Year"] <- 1980
author_analysis[361, "Time"] <- 1

author_analysis[371, "Citations"] <- 2
author_analysis[371, "Year"] <- 2022
author_analysis[371, "Time"] <- 5

author_analysis[374, "Citations"] <- 7
author_analysis[374, "Year"] <- 2019
author_analysis[374, "Time"] <- 4

author_analysis[375, "Year"] <- 2019
author_analysis[375, "Time"] <- 4
author_analysis[375, "Citations"] <- 18

author_analysis[283, "Journal"] <- "Games and Economic Behavior"
author_analysis[285, "Journal"] <- "Games and Economic Behavior"
author_analysis[286, "Journal"] <- "Games and Economic Behavior"
author_analysis[290, "Journal"] <- "European Journal of Operational Research"
author_analysis[292, "Journal"] <- "European Journal of Operational Research"
author_analysis[293, "Journal"] <- "European Journal of Operational Research"
author_analysis[294, "Journal"] <- "European Journal of Operational Research"
author_analysis[297, "Journal"] <- "Economics Letters"
author_analysis[299, "Journal"] <- "Decision Support Systems"
author_analysis[300, "Journal"] <- "Decision Support Systems"
author_analysis[301, "Journal"] <- "Decision Support Systems"
author_analysis[302, "Journal"] <- "Decision Support Systems"
author_analysis[303, "Journal"] <- "Decision Support Systems"
author_analysis[304, "Journal"] <- "Decision Support Systems"
author_analysis[306, "Journal"] <- "Decision Support Systems"
author_analysis[307, "Journal"] <- "Decision Support Systems"
author_analysis[312, "Journal"] <- "Computers in Human Behavior"
author_analysis[314, "Journal"] <- "Organizational Behavior and Human Decision Processes"
author_analysis[315, "Journal"] <- "The Journal of Product Innovation Management"
author_analysis[318, "Journal"] <- "Journal of Mathematical Economics"
author_analysis[319, "Journal"] <- "Journal of Monetary Economics"
author_analysis[327, "Journal"] <- "International Journal of Research in Marketing"
author_analysis[328, "Journal"] <- "Journal of Environmental Economics and Management"
author_analysis[330, "Journal"] <- "Journal of Retailing"
author_analysis[331, "Journal"] <- "Industrial Marketing Management"
author_analysis[332, "Journal"] <- "Electronic Markets"
author_analysis[333, "Journal"] <- "Journal of the Academy of Marketing Science"
author_analysis[335, "Journal"] <- "Journal of the Academy of Marketing Science"
author_analysis[336, "Journal"] <- "Journal of the Academy of Marketing Science"
author_analysis[338, "Journal"] <- "Quantitative Marketing and Economics"
author_analysis[339, "Journal"] <- "Quantitative Marketing and Economics"
author_analysis[341, "Journal"] <- "Marketing Letters"
author_analysis[342, "Journal"] <- "Marketing Letters"
author_analysis[343, "Journal"] <- "Marketing Letters"
author_analysis[344, "Journal"] <- "Marketing Letters"
author_analysis[345, "Journal"] <- "Marketing Letters"
author_analysis[346, "Journal"] <- "Marketing Letters"
author_analysis[348, "Journal"] <- "Marketing Letters"
author_analysis[350, "Journal"] <- "Information Systems Frontiers"

author_analysis_noNA <-NA
author_analysis_noNA <- na.omit(author_analysis)

### ANALYSIS
#how many citations do the authors have in total
author_analysis_noNA %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations))
author_analysis_noNA %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>% top_n(20, Freq)
author_analysis_noNA %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>% arrange(desc(Freq)) %>% top_n(n=25)
authors_with_most_citations <- as.data.frame(author_analysis_noNA %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>%  arrange(desc(Freq)) %>% top_n(n=25))

mostcitedauthorsplot <- authors_with_most_citations %>%
  ggplot(aes(x=reorder(Author,Freq), y=Freq)) + geom_bar(stat='identity') + coord_flip() + theme_grey() + labs( x = 'Author', y = 'Number of citations') +ggtitle('Most cited authors')

#how many papers do the authors have in total
author_analysis_noNA %>% dplyr::count(Author) %>%arrange(desc(n)) %>% filter(n>1)
authors_with_most_articles <- as.data.frame(author_analysis_noNA %>% dplyr::count(Author) %>% filter(n>2) %>% arrange(desc(n)))
author_analysis_noNA %>% filter(Author %in% authors_with_most_articles$Author) %>% ggplot(aes(x = Author)) + geom_bar() + coord_flip() + theme_bw() + labs( x = '', y = 'Number of publications') 

mostpublishedauthorsplots <- authors_with_most_articles  %>% ggplot(aes(x=reorder(Author,n), y=n)) +
  geom_bar(stat='identity') + coord_flip() + theme_bw() + labs(x='Author', y='Number of articles')

##how many papers do first authors have in total
first_authors_with_most_articles <- as.data.frame(author_analysis_noNA %>% filter(Number==1) %>% dplyr::count(Author) %>% filter(n>1))
mostpublishedfirstauthorplots <- first_authors_with_most_articles %>% ggplot(aes(x=reorder(Author, n), y=n)) +
  geom_bar(stat='identity') + coord_flip() + theme_bw() + labs(x='Author', y='Number of articles') + ggtitle('First authors with more than two publications')

## TIME ANALYSIS

#get list of most published & cited authors per timeframe
##timeframe <1990
mostpubs_1990plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="1") %>% dplyr::count(Author) %>% filter(n>1)) %>%
  ggplot(aes(x=reorder(Author, n), y=n)) +
  geom_bar(stat='identity',fill='aquamarine4') + coord_flip() + theme_grey() + labs(x='Author', y='Number of articles') + 
  ggtitle("Authors with more than 2 publications in <1990")

mostcites_1990plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="1") %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>%  arrange(desc(Freq)) %>% top_n(n=25)) %>%
  ggplot(aes(x=reorder(Author,Freq), y=Freq)) + 
  geom_bar(stat='identity', fill='aquamarine4') + coord_flip() + theme_grey() + labs( x = 'Author', y = 'Number of citations') + 
  ggtitle('Most cited authors in <1990')

#timeframe 1999
mostpubs_1999plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="2") %>% dplyr::count(Author) %>% filter(n>1)) %>%
  ggplot(aes(x=reorder(Author, n), y=n)) +
  geom_bar(stat='identity', fill='cyan4') + coord_flip() + theme_grey() + labs(x='Author', y='Number of articles') + 
  ggtitle("Authors with more than 2 publications in 1990-1999")

mostcites_1999plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="2") %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>%  arrange(desc(Freq)) %>% top_n(n=25)) %>%
  ggplot(aes(x=reorder(Author,Freq), y=Freq)) + 
  geom_bar(stat='identity', fill='cyan4') + coord_flip() + theme_grey() + labs( x = 'Author', y = 'Number of citations') + 
  ggtitle('Most cited authors in 1990-1999')

#timeframe 2009
mostpubs_2009plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="3") %>% dplyr::count(Author) %>% filter(n>1)) %>%
  ggplot(aes(x=reorder(Author, n), y=n)) +
  geom_bar(stat='identity', fill='darkseagreen') + coord_flip() + theme_grey() + labs(x='Author', y='Number of articles') + 
  ggtitle("Authors with more than 2 publications in 2000-2009")

mostcites_2009plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="3") %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>%  arrange(desc(Freq)) %>% top_n(n=25)) %>%
  ggplot(aes(x=reorder(Author,Freq), y=Freq)) + 
  geom_bar(stat='identity', fill='darkseagreen') + coord_flip() + theme_grey() + labs( x = 'Author', y = 'Number of citations') + 
  ggtitle('Most cited authors in 2000-2009')

#timeframe 2019
mostpubs_2019plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="4") %>% dplyr::count(Author) %>% filter(n>1)) %>%
  ggplot(aes(x=reorder(Author, n), y=n)) +
  geom_bar(stat='identity', fill='darkslategray4') + coord_flip() + theme_grey() + labs(x='Author', y='Number of articles') + 
  ggtitle("Authors with more than 2 publications in 2010-2019")

mostcites_2019plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="4") %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>%  arrange(desc(Freq)) %>% top_n(n=25)) %>%
  ggplot(aes(x=reorder(Author,Freq), y=Freq)) + 
  geom_bar(stat='identity', fill='darkslategray4') + coord_flip() + theme_grey() + labs( x = 'Author', y = 'Number of citations') + 
  ggtitle('Most cited authors in 2010-2019')

#timeframe 2022
mostpubs_2022plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="5") %>% dplyr::count(Author) %>% filter(n>1)) %>%
  ggplot(aes(x=reorder(Author, n), y=n)) +
  geom_bar(stat='identity', fill='darkolivegreen') + coord_flip() + theme_grey() + labs(x='Author', y='Number of articles') + 
  ggtitle("Authors with more than 2 publications in 2020-2022")

mostcites_2022plot <- as.data.frame(author_analysis_noNA %>% filter(Time=="5") %>% group_by(Author) %>% dplyr::summarise(Freq = sum(Citations)) %>%  arrange(desc(Freq)) %>% top_n(n=25)) %>%
  ggplot(aes(x=reorder(Author,Freq), y=Freq)) + 
  geom_bar(stat='identity', fill='darkolivegreen') + coord_flip() + theme_grey() + labs( x = 'Author', y = 'Number of citations') + 
  ggtitle('Most cited authors in 2020-2022')

#### CREATING AUTHOR EDGES FOR GRAPH ANALYSIS

#i = row_number
author_edgelist <- data.frame(title=NA,journal=NA,year=NA,citations=NA,time=NA,author1=NA,author2=NA)
author_analysis_noNA <- as.data.frame(author_analysis_noNA)
for( i in 1:nrow(author_analysis_noNA)){
  list_authors <- str_split(author_analysis_noNA[i,"Authors"], "\\s")[[1]]
  if (length(list_authors) > 1){
    title = author_analysis_noNA[i,"Title"]
    journal = author_analysis_noNA[i,"Journal"]
    year = author_analysis_noNA[i,"Year"]
    citations = author_analysis_noNA[i,"Citations"]
    time = author_analysis_noNA[i,"Time"]
    for (j in list_authors[-length(list_authors)]){
      element1 = j
      k = match(element1,list_authors)[[1]]+1
      sublist = list_authors[k:length(list_authors)]
      for (l in sublist){
        element2 = l
        new_row = c(title,journal,year,citations,time,element1,element2)
        print(new_row)
        author_edgelist <- rbind(author_edgelist,new_row)
      }
    }
  }
}
author_edgelist <- author_edgelist[-1,]
write.csv(author_edgelist,"/Users/emiliadecoene/Desktop/thesis/authoredgelist.csv", row.names = TRUE)


author_edgelist <- read.csv("/Users/emiliadecoene/Desktop/thesis/authoredgelist.csv", header=TRUE)
author_edgelist <- author_edgelist[,-1]


