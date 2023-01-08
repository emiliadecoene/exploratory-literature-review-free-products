### R SCRIPT: Exploratory Literature on Digital & Physical Free Goods through Time Using Topic Models
## install packages
library(fulltext)
library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)
library(ggwordcloud)
library(data.table)
library(SnowballC)
gctorture(on = FALSE)
library(tm)
library(topicmodels)
library(plyr)
library(ldatuning)
library(textmineR)
library(ggthemes)
library(plotly)
library(stm)
library(stmCorrViz)
library(lubridate)
library(scales)

### ----- DATA COLLECTION ------------------------------------------------------------------------ ###

##----------------------------------------------------------------------------------------------------
# 1) Scopus search
##----------------------------------------------------------------------------------------------------

# Useful pages: https://dev.elsevier.com/sc_search_tips.html
#               https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl

#You need to get API keys
opts   <- list(key='2a09946f4aea17d5acd20a9007285cdf')
msopts <- list(key='Ed34447')

#Retrieve articles from Scopus Database
query <- paste0(
  '( TITLE-ABS-KEY("product sampling") OR TITLE-ABS-KEY("free to fee") OR TITLE-ABS-KEY("paywall") 
  OR TITLE-ABS-KEY("free digital content") OR TITLE-ABS-KEY("free service") OR TITLE-ABS-KEY("free gifts") 
  OR TITLE-ABS-KEY("nonprice promotion") OR TITLE-ABS-KEY("zero price") OR TITLE-ABS-KEY("zero pricing") 
  OR TITLE-ABS-KEY("free trial") OR TITLE-ABS-KEY("free sampling") OR TITLE-ABS-KEY("freemium") 
  OR TITLE-ABS-KEY("free sample") OR TITLE-ABS-KEY("free product") OR TITLE-ABS-KEY("product trial") 
  OR TITLE-ABS-KEY("free content") OR TITLE-ABS-KEY("free version")) AND ',
  '(DOCTYPE(ar) OR DOCTYPE(cp)) AND', '(LANGUAGE(english)) AND', '(SUBJAREA(BUSI) OR SUBJAREA(SOCI) 
  OR SUBJAREA(COMP) OR SUBJAREA(ECON)
    OR SUBJAREA(DECI))')
res <- ft_search(query, from=c('scopus'), scopusopts = opts, limit = 5000)

#Create proper data table "tb.allarticles" containing metadata of the retrieved articles
tb.allarticles <- res$scopus$data %>% 
  mutate( 
    cal_year = format(as.Date(`prism:coverDate`), '%Y')
    , type = subtypeDescription
    , scopid = `dc:identifier`
    , doi  = `prism:doi`
    , journal = `prism:publicationName`
    , title   = `dc:title`
    , author  = `dc:creator`
    , citations = as.double(`citedby-count`)
  ) %>%  select(cal_year, type, scopid, doi, journal, title, author, citations)

##----------------------------------------------------------------------------------------------------
## 2) Filter data and keep most relevant articles
##----------------------------------------------------------------------------------------------------

#distribution of article citations: 
tb.allarticles$citations <- as.numeric(tb.allarticles$citations)
ggplot(tb.allarticles, aes(x=citations)) + geom_histogram()
0.05*nrow(tb.allarticles) 
tb.allarticles[tb.allarticles$citations>65,.N] ##only 5% of the papers has more than 65 citations

#we keep the articles that originate from high-valued journals or if they are in the top 5% most cited articles
# and store the filtered set of articles in "tb.filtered_articles"
high_ranked_journals_list <- read_csv("/Users/emiliadecoene/Desktop/thesis/CODE/high_ranked_journals.csv")
tb.filtered_articles <- filter(tb.allarticles,journal %in% high_ranked_journals_list$high_ranked_journals | citations > 65)

##----------------------------------------------------------------------------------------------------
## 3) Fetch abstracts
##----------------------------------------------------------------------------------------------------

# useful page: #https://rdrr.io/cran/fulltext/man/ft_abstract.html

#split into smaller datasets
#--pt1
tb.articles_pt1 <- tb.filtered_articles[1:100,]
dois_pt1 <- na.omit(tb.articles_pt1$doi)
out_pt1 <- ft_abstract(x=dois_pt1, from = 'scopus', scopusopts = opts2)
list_of_abstracts_pt1 <- out_pt1$scopus
tb.abstracts_all <- NULL
for( i in 1:length(list_of_abstracts_pt1)){
  print(i)
  if(!is.null(list_of_abstracts_pt1[[i]]$abstract)){
    tb.row <- data.frame(doi = paste0(list_of_abstracts_pt1[[i]]$doi), abstract = paste0(list_of_abstracts_pt1[[i]]$abstract))
    tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)
  }
}

#--pt2
tb.articles_pt2 <- tb.filtered_articles[101:200,]
dois_pt2 <- na.omit(tb.articles_pt2$doi)
out_pt2 <- ft_abstract(x=dois_pt2, from = c('scopus'), scopusopts = opts)
list_of_abstracts_pt2 <- out_pt2$scopus
for( i in 1:length(list_of_abstracts_pt2)){
  print(i)
  if(!is.null(list_of_abstracts_pt2[[i]]$abstract)){
    tb.row <- data.frame(doi = paste0(list_of_abstracts_pt2[[i]]$doi), abstract = paste0(list_of_abstracts_pt2[[i]]$abstract))
    tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)
  }
}

#--pt3
tb.articles_pt3 <- tb.filtered_articles[201:300,]
dois_pt3 <- na.omit(tb.articles_pt3$doi)
out_pt3 <- ft_abstract(x=dois_pt3, from = c('scopus'), scopusopts = opts)
list_of_abstracts_pt3 <- out_pt3$scopus
for( i in 1:length(list_of_abstracts_pt3)){
  print(i)
  if(!is.null(list_of_abstracts_pt3[[i]]$abstract)){
    tb.row <- data.frame(doi = paste0(list_of_abstracts_pt3[[i]]$doi), abstract = paste0(list_of_abstracts_pt3[[i]]$abstract))
    tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)
  }
}

#--pt4
tb.articles_pt4 <- NULL
dois_pt4 <- NULL
tb.articles_pt4 <- tb.filtered_articles[303:320,]
dois_pt4 <- na.omit(tb.articles_pt4$doi)
out_pt4 <- ft_abstract(x=dois_pt4, from = c('scopus'), scopusopts = opts)
list_of_abstracts_pt4 <- out_pt4$scopus
for( i in 1:length(list_of_abstracts_pt4)){
  print(i)
  if(!is.null(list_of_abstracts_pt4[[i]]$abstract)){
    tb.row <- data.frame(doi = paste0(list_of_abstracts_pt4[[i]]$doi), abstract = paste0(list_of_abstracts_pt4[[i]]$abstract))
    tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)
  }
}

#--pt5
tb.articles_pt5 <- NULL
dois_pt5 <- NULL
tb.articles_pt5 <- tb.filtered_articles[321:324,]
dois_pt5 <- na.omit(tb.articles_pt5$doi)
out_pt5 <- ft_abstract(x=dois_pt5, from = c('scopus'), scopusopts = opts)
list_of_abstracts_pt5 <- out_pt5$scopus
for( i in 1:length(list_of_abstracts_pt5)){
  print(i)
  if(!is.null(list_of_abstracts_pt5[[i]]$abstract)){
    tb.row <- data.frame(doi = paste0(list_of_abstracts_pt5[[i]]$doi), abstract = paste0(list_of_abstracts_pt5[[i]]$abstract))
    tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)
  }
}

# --pt 6
tb.articles_pt6 <- NULL
dois_pt6 <- NULL
tb.articles_pt6 <- tb.filtered_articles[326:393,]
dois_pt6 <- na.omit(tb.articles_pt6$doi)
out_pt6 <- ft_abstract(x=dois_pt6, from = c('scopus'), scopusopts = opts)
list_of_abstracts_pt6 <- out_pt6$scopus
for( i in 1:length(list_of_abstracts_pt6)){
  print(i)
  if(!is.null(list_of_abstracts_pt6[[i]]$abstract)){
    tb.row <- data.frame(doi = paste0(list_of_abstracts_pt6[[i]]$doi), abstract = paste0(list_of_abstracts_pt6[[i]]$abstract))
    tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)
  }
}

#abstract 301 is missing
out_301 <- ft_abstract(x=tb.filtered_articles[301,]$doi, from = c('scopus'), scopusopts = opts)
abstract_301 <- out_301$scopus
tb.row <- data.frame(doi=paste0(abstract_301[[1]]$doi), abstract=paste0(abstract_301[[1]]$abstract))
tb.abstracts_all <- rbind(tb.abstracts_all, tb.row)

#other missing abstracts
tb.database[310, "abstract"] <- "Two experiments examine the process by which free gift promotions serve as a 
                                source of information about the underlying value of the product offered as a 
                                free gift. The value-discounting hypothesis argues that by virtue of being offered 
                                as a free gift, products will be valued less as evinced by lower purchase intentions and a 
                                lower price that consumers are willing to pay for them. Conditions that inhibit the 
                                value-discounting effect include the (a) presence of alternate price information to make 
                                judgments about the value of the gift, and (b) contextual information about the value of 
                                the promoted brand."
tb.database[312, "abstract"] <- "This paper focuses on the pricing aspect of the net neutrality debate, in particular, 
                                the de facto ban on fees levied by Internet service providers on content providers to reach users. 
                                This zero-price rule may prove desirable for several reasons. Using a two-sided market analysis, 
                                we suggest that it subsidizes creativity and innovation in new content creation, goals shared by copyright 
                                and patent laws. The rule also helps to solve a coordination problem: since Internet service providers do not 
                                completely internalize the effects of their own pricing decisions, lack of regulation may lead to even higher 
                                fees charged by all. Finally, allowing for such fees runs the risk of creating horizontally differentiated 
                                Internet service providers with different libraries of accessible content, thereby foreclosing consumers and 
                                leading to Internet fragmentation."
tb.database[388, "abstract"] <- "Most online services (Google, Facebook etc.) operate by providing a service to users for free, 
                                and in return they collect and monetize personal information (PI) of the users. This operational model is 
                                inherently economic, as the good being traded and monetized is PI. This model is coming under increased 
                                scrutiny as online services are moving to capture more PI of users, raising serious privacy concerns. 
                                However, little is known on how users valuate different types of PI while being online, as well as the 
                                perceptions of users with regards to exploitation of their PI by online service providers. In this paper, 
                                we study how users valuate different types of PI while being online, while capturing the context by relying 
                                on Experience Sampling. We were able to extract the monetary value that 168 participants put on different 
                                pieces of PI. We find that users value their PI related to their offline identities more (3 times) than 
                                their browsing behavior. Users also value information pertaining to financial transactions and social network 
                                interactions more than activities like search and shopping. We also found that while users are overwhelmingly 
                                in favor of exchanging their PI in return for improved online services, they are uncomfortable if these same 
                                providers monetize their PI."
tb.database[393, "abstract"] <- "When products become commodities, manufacturing companies may seek to differentiate themselves with value-added services—a potentially profitable strategy. Unfortunately, companies often stumble in the effort. Reinartz and Ulaga conducted in-depth studies of 18 leading companies in a broad variety of product markets to learn what distinguished the successes from the rest. They discovered four steps to developing a profitable services capability.
                                Recognize that you already have a service company. You can identify and charge for simple services—as Merck 
                                did when it stopped quietly absorbing shipping costs. Switching services from free to fee clarifies their value 
                                for managers as well as for customers. Industrialize the back office. To prevent delivery costs from eating 
                                up service-offering margins, build flexible service platforms, closely monitor process costs, and exploit new 
                                technologies that enable process innovations. The Swedish bearings manufacturer SKF provided off-site access to 
                                an online monitoring tool that could warn of potential failure in customers’ machines. Create a service-savvy 
                                sales force. Services require longer sales cycles and, often, decisions from high up in a customer’s hierarchy;
                                what’s more, product salespeople may be inimical to change. Schneider-Electric did a major overhaul of its sales
                                organization and trained its people to switch from cost-plus pricing to value-based pricing. Focus on customers’
                                processes and the opportunities they afford for new service offerings. You may need to acquire new capabilities
                                to take advantage of those opportunities: The industrial coatings specialist PPG had to learn how painting 
                                robots function after it offered to take over Fiat’s Torino paint shop. Services can both lock in customers and
                                help acquire new accounts. They should be developed with care and attention"

#everything together
tb.total_all <- rbind(tb.filtered_articles)
tb.database <- merge(tb.abstracts_all, tb.total_all, by="doi", all=TRUE)

##----------------------------------------------------------------------------------------------------
## 3) Check titles and abstracts manually to see get rid of irrelevant articles
##----------------------------------------------------------------------------------------------------

# create column "approved" that assigns a value to an article, TRUE if we keep the article, FALSE if we don't keep the article
#   because it's irrelevant (see reason why in comments)
tb.database$approved <- NA
tb.database[17, "approved"] <- FALSE #physics
tb.database[18, "approved"] <- TRUE  #econometrics, economic theory
tb.database[19, "approved"] <- FALSE #mathematics
tb.database[20, "approved"] <- FALSE #mathematics 
tb.database[21, "approved"] <- FALSE #mathematics
tb.database[28, "approved"] <- FALSE #econometrics, economic theory
tb.database[31, "approved"] <- FALSE #public sector
tb.database[32, "approved"] <- FALSE #mathematics, optimization problems
tb.database[33, "approved"] <- FALSE #health insurance, healthcare industry
tb.database[34, "approved"] <- FALSE #operations research, inventory management
tb.database[35, "approved"] <- FALSE #ecology
tb.database[37, "approved"] <- FALSE #healthcare industry
tb.database[46, "approved"] <- FALSE #physics
tb.database[60, "approved"] <- FALSE #healthcare industry
tb.database[24, "approved"] <- FALSE #public sector
tb.database[22, "approved"] <- FALSE #public sector
tb.database[57, "approved"] <- FALSE #public sector
tb.database[61, "approved"] <- FALSE #health insurance, healthcare industry
tb.database[62, "approved"] <- FALSE #health insurance, healthcare industry
tb.database[63:65, "approved"] <- FALSE #health insurance, healthcare industry
tb.database[66, "approved"] <- FALSE #labour economics
tb.database[67, "approved"] <- FALSE #finance, economic theory, banking
tb.database[69, "approved"] <- FALSE #mathematics
tb.database[73, "approved"] <- FALSE #mathematics
tb.database[76, "approved"] <- FALSE #public sector
tb.database[77, "approved"] <- FALSE #operations research, inventory management
tb.database[78, "approved"] <- FALSE #mathematics, statistics
tb.database[88, "approved"] <- FALSE #ecology
tb.database[90, "approved"] <- FALSE #retail: not about zero prices
tb.database[91, "approved"] <- FALSE #econometrics, economic theory
tb.database[95, "approved"] <- FALSE #operations research, electricity markets
tb.database[97, "approved"] <- FALSE #public sector
tb.database[98, "approved"] <- FALSE #economic theory
tb.database[99, "approved"] <- FALSE #operations research
tb.database[102, "approved"] <- FALSE #economic theory
tb.database[131, "approved"] <- FALSE #healthcare industry
tb.database[132, "approved"] <- FALSE #healthcare industry
tb.database[135, "approved"] <- FALSE #health insurance, healthcare industry
tb.database[136, "approved"] <- FALSE #insurance
tb.database[139, "approved"] <- FALSE #mathematics, statistics
tb.database[140, "approved"] <- FALSE #mathematics, statistics
tb.database[141, "approved"] <- FALSE #mathematics, statistics
tb.database[142, "approved"] <- FALSE #economic theory
tb.database[143, "approved"] <- FALSE #mathematics, statistics
tb.database[153, "approved"] <- FALSE #software development, operations research
tb.database[155, "approved"] <- FALSE #neurocomputations, mathematics
tb.database[157:160, "approved"] <- FALSE #public sector, healthcare industry
tb.database[161, "approved"] <- FALSE #operations research
tb.database[162, "approved"] <- FALSE #public sector
tb.database[167, "approved"] <- FALSE #mathematics, statistics
tb.database[170, "approved"] <- FALSE #ecology
tb.database[176, "approved"] <- FALSE #sociology
tb.database[177, "approved"] <- FALSE 
tb.database[181, "approved"] <- FALSE #product warranty
tb.database[185, "approved"] <- FALSE #public sector
tb.database[186, "approved"] <- FALSE #mathematics, statistics
tb.database[189, "approved"] <- FALSE #economy theory
tb.database[201, "approved"] <- FALSE ##mathematics, statistics
tb.database[208, "approved"] <- FALSE #healthcare industry
tb.database[214, "approved"] <- FALSE #quality
tb.database[217, "approved"] <- FALSE #healthcare industry
tb.database[220, "approved"] <- FALSE #healthcare industry
tb.database[221, "approved"] <- FALSE #healthcare industry
tb.database[222, "approved"] <- FALSE #ecology, agriculture
tb.database[223, "approved"] <- FALSE #ecology
tb.database[225, "approved"] <- FALSE #insurance
tb.database[226, "approved"] <- FALSE #economic theory
tb.database[228, "approved"] <- FALSE #operations research, retail
tb.database[231, "approved"] <- FALSE #healthcare industry
tb.database[244, "approved"] <- FALSE #operations research, quality
tb.database[245, "approved"] <- FALSE #operations research, software development
tb.database[246, "approved"] <- FALSE #operations research, software development
tb.database[249, "approved"] <- FALSE #physics
tb.database[250, "approved"] <- FALSE #physics
tb.database[251, "approved"] <- FALSE #quality management
tb.database[255, "approved"] <- FALSE #mathematics, statistics
tb.database[256, "approved"] <- FALSE #mathematics, statistics
tb.database[257, "approved"] <- FALSE #mathematics, statistics
tb.database[258, "approved"] <- FALSE #mathematics, statistics
tb.database[260, "approved"] <- FALSE #agriculture
tb.database[262, "approved"] <- FALSE #public sector
tb.database[269, "approved"] <- FALSE #economic theory
tb.database[270, "approved"] <- FALSE #mathematics, statistics
tb.database[271, "approved"] <- FALSE #ecology
tb.database[272, "approved"] <- FALSE #economic theory
tb.database[273, "approved"] <- FALSE #financial mathematics
tb.database[282, "approved"] <- FALSE #tobacco industry
tb.database[283, "approved"] <- FALSE #tobacco industry
tb.database[284, "approved"] <- FALSE #mathematics
tb.database[285, "approved"] <- FALSE #physics
tb.database[286, "approved"] <- FALSE #human-computing interaction, computing
tb.database[287, "approved"] <- FALSE #computing
tb.database[302, "approved"] <- FALSE #mathematics, statistics
tb.database[303, "approved"] <- FALSE #healthcare industry
tb.database[304, "approved"] <- FALSE #operations research, quality
tb.database[306, "approved"] <- FALSE #tobacco industry
tb.database[315, "approved"] <- FALSE #economic theory
tb.database[363, "approved"] <- FALSE #neurocomputing, mathematics, statistics
tb.database[364, "approved"] <- FALSE #human computer interaction, mathematics, statistics
tb.database[365, "approved"] <- FALSE #economic theory
tb.database[366, "approved"] <- FALSE #economic theory, ecology
tb.database[367, "approved"] <- FALSE #healthcare industry
tb.database[369, "approved"] <- FALSE #economic theory
tb.database[370, "approved"] <- FALSE #ecology
tb.database[385, "approved"] <- FALSE #physics
tb.database[386, "approved"] <- FALSE #food industry
tb.database[389, "approved"] <- FALSE #mathematics, computing
tb.database[390, "approved"] <- FALSE #mathematics, computing
tb.database[391, "approved"] <- FALSE #mathematics, computing
tb.database[392, "approved"] <- FALSE #mathematics, computing

#we create a final database "tb.truedatabase" that holds only the relevant papers
tb.database$approved[is.na(tb.database$approved)] <- TRUE
tb.truedatabase <- NULL
tb.truedatabase <- tb.database %>% filter(approved == TRUE) ## 279 articles left

##----------------------------------------------------------------------------------------------------
## 4) save datasets
##----------------------------------------------------------------------------------------------------

write.csv(tb.database,"~/thesis/trueandfalsedatabase.csv", row.names = FALSE)
write.csv(tb.truedatabase,"~/thesis/truedatabase.csv", row.names = FALSE)



### ----- AUTHOR ANALYSIS ------------------------------------------------------------ ###
authors_with_most_articles <- as.data.frame(tb.truedatabase%>% dplyr::count(author) %>%arrange(desc(n)))
##see extra R script

### ----- JOURNAL ANALYSIS ------------------------------------------------------------ ###
#how many articles per journal
#group by journal, count nr of articles
tb.truedatabase %>% dplyr::count(journal) %>%arrange(desc(n))
#creating barplot with journals having more than 5 articles
journals_count <- as.data.frame(tb.truedatabase%>% dplyr::count(journal) %>%arrange(desc(n)))
journals_with_most_articles <- as.data.frame(tb.truedatabase%>% dplyr::count(journal) %>%arrange(desc(n)) %>% top_n(n = 20))
journals_with_most_articles %>% 
  ggplot(aes(x=reorder(journal,n), y=n)) + geom_bar(stat="identity") + 
  coord_flip() + theme_grey() + labs( x = 'Journal name', y = 'Number of publications') + ylim(0,15)
unique(tb.data$journal)


### ----- STRUCTURED TOPIC MODEL ----------------------------------------------------------------- ###

##----------------------------------------------------------------------------------------------------
## 1) set up
##----------------------------------------------------------------------------------------------------

# we want to create an stm that has a prevalent covariate of time, so that we can see 
# how the different models change over time

# first we have a closer look at the cal_year variable

tb.test <- tb.truedatabase
tb.test$cal_year <- as.Date(as.character(tb.test$cal_year), "%Y")
tb.test %>% 
  ggplot(aes(x=cal_year)) +geom_line(stat="count") + 
  theme_bw() + ggtitle("Evolution of free product literature") + 
  ylab("Number of published articles") + xlab("Year")

# we create a time dummy variable to use as prevalent covariate
tb.truedatabase$time <- NA
for(i in 1:nrow(tb.truedatabase)) {
  if(tb.truedatabase$cal_year[i] > 2019){
    tb.truedatabase$time[i] <- "2020-2022" 
  } else if(tb.truedatabase$cal_year[i] > 2009) {
    tb.truedatabase$time[i] <- "2010-2019"
  } else if(tb.truedatabase$cal_year[i] > 1999) {
    tb.truedatabase$time[i] <- "2000-2009"
  } else if(tb.truedatabase$cal_year[i] > 1989) {
    tb.truedatabase$time[i] <- "1990-1999"
  } else {
    tb.truedatabase$time[i] <- "<1990"
  }
}
tb.truedatabase$time <- as.factor(tb.truedatabase$time)
table(tb.truedatabase$time) 
tb.truedatabase %>% ggplot(aes(x=time)) + geom_bar(stat="count") +labs(x="Time interval", y="Number of articles") + theme_bw()

vector <- c(11/14,21/10, 47/10, 141/10, 59/3)
time_vector <- c("<1990", "1990-1999", "2000-2009", "2010-2019", "2020-2022")
time_intervals_yearly <- as.data.frame(cbind(interval=time_vector, yearly_value=vector))
time_intervals_yearly$yearly_value <- as.numeric(time_intervals_yearly$yearly_value)
time_intervals_yearly %>% ggplot(aes(x=interval, y=yearly_value)) + geom_bar(stat="identity") +labs(x="Time interval", y="Number of articles divided by interval width") + ylim(c(0,25)) + theme_bw()

##----------------------------------------------------------------------------------------------------
## 2) structured topic model: time interval covariate, with stemming and with removing stop words
##----------------------------------------------------------------------------------------------------

# source: https://juliasilge.com/blog/evaluating-stm/
#prepare dtm

processed <- textProcessor(tb.truedatabase$abstract, metadata=tb.truedatabase)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab #-> original words
meta <- out$meta #-> original dataset, used for getting the covariate


# creating a lot of different topic models with different number of topics, including the time covariate
k_seq <- seq(3,30)
storage <- searchK(docs, vocab, K=k_seq, 
                   prevalence= ~ time, data=meta, cores = 2)

# evaluate the models by held-out likelihood, semantic coherence, residuals & lower bound
print(storage)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage) # --> looking at the residuals & semantic coherence (elbow), the optimal K is between 11 and 15


# we fit the models with K between 10 and 15 

abstractfit_k10 <- stm(documents = out$documents, vocab=out$vocab,
                       K=10, prevalence=~time, data=out$meta, init.type="Spectral")

abstractfit_k11 <- stm(documents = out$documents, vocab=out$vocab,
                       K=11, prevalence=~time, data=out$meta, init.type="Spectral")

abstractfit_k12 <- stm(documents = out$documents, vocab=out$vocab,
                       K=12, prevalence=~time, data=out$meta, init.type="Spectral")

abstractfit_k13 <- stm(documents = out$documents, vocab=out$vocab,
                       K=13, prevalence=~time, data=out$meta, init.type="Spectral")

abstractfit_k14 <- stm(documents = out$documents, vocab=out$vocab,
                       K=14, prevalence=~time, data=out$meta, init.type="Spectral")

abstractfit_k15 <- stm(documents = out$documents, vocab=out$vocab,
                       K=15, prevalence=~time, data=out$meta, init.type="Spectral")

# we zoom in on the trade off between exclusivity and semantic coherence for these models
# we want the model that has the highest exclusiveness and coherence at the same time

M10ExSem<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10), semanticCoherence(model=abstractfit_k10, documents = out$documents), "Mod10"))
M11ExSem<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11), semanticCoherence(model=abstractfit_k11, documents = out$documents), "Mod11"))
M12ExSem<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12), semanticCoherence(model=abstractfit_k12, documents = out$documents), "Mod12"))
M13ExSem<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13), semanticCoherence(model=abstractfit_k13, documents = out$documents), "Mod13"))
M14ExSem<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14), semanticCoherence(model=abstractfit_k14, documents = out$documents), "Mod14"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15), semanticCoherence(model=abstractfit_k15, documents = out$documents), "Mod15"))

ModsExSem<-rbind(M10ExSem, M11ExSem, M12ExSem, M13ExSem, M14ExSem, M15ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model)) + geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer # --> model 10 or model 11 seems to be best

# zoom in on model 12
ModsExSem %>% filter(Model %in% c("Mod10","Mod11","Mod12", "Mod13")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 13 is the best
# evaluation metrics: 
# 13 9.139672 -69.85394 -6.822289 1.515346

##----------------------------------------------------------------------------------------------------
## 3) structured topic model: time interval covariate, without stemming
##----------------------------------------------------------------------------------------------------

#prepare dtm

processed2 <- textProcessor(tb.truedatabase$abstract, metadata=tb.truedatabase, stem = FALSE)
out2 <- prepDocuments(processed2$documents, processed2$vocab, processed2$meta)
docs2 <- out2$documents
vocab2 <- out2$vocab
meta2 <- out2$meta

# creating a lot of different topic models with different number of topics, including the time covariate

k_seq <- seq(3,30)
storage2 <- searchK(out2$documents, out2$vocab, K=k_seq, 
                    prevalence= ~ time, data=meta2, cores = 2)
print(storage2)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage2) #clearly in between 9 and 15 based on residuals and semantic coherence

# we fit the models with K between 9 and 15 

abstractfit_k9_2 <- stm(documents = docs2, vocab=vocab2,
                        K=9, prevalence=~time, data=meta2, init.type="Spectral")

abstractfit_k10_2 <- stm(documents = docs2, vocab=vocab2,
                         K=10, prevalence=~time, data=meta2, init.type="Spectral")

abstractfit_k11_2 <- stm(documents = docs2, vocab=vocab2,
                         K=11, prevalence=~time, data=meta2, init.type="Spectral")

abstractfit_k12_2 <- stm(documents = docs2, vocab=vocab2,
                         K=12, prevalence=~time, data=meta2, init.type="Spectral")

abstractfit_k13_2 <- stm(documents = docs2, vocab=vocab2,
                         K=13, prevalence=~time, data=meta2, init.type="Spectral")

abstractfit_k14_2 <- stm(documents = docs2, vocab=vocab2,
                         K=14, prevalence=~time, data=meta2, init.type="Spectral")

abstractfit_k15_2 <- stm(documents = docs2, vocab=vocab2,
                         K=15, prevalence=~time, data=meta2, init.type="Spectral")

M9ExSem_2<-as.data.frame(cbind(c(1:9),exclusivity(abstractfit_k9_2), semanticCoherence(model=abstractfit_k9_2, documents = out2$documents), "Mod9"))
M10ExSem_2<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_2), semanticCoherence(model=abstractfit_k10_2, documents = out2$documents), "Mod10"))
M11ExSem_2<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_2), semanticCoherence(model=abstractfit_k11_2, documents = out2$documents), "Mod11"))
M12ExSem_2<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_2), semanticCoherence(model=abstractfit_k12_2, documents = out2$documents), "Mod12"))
M13ExSem_2<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_2), semanticCoherence(model=abstractfit_k13_2, documents = out2$documents), "Mod13"))
M14ExSem_2<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_2), semanticCoherence(model=abstractfit_k14_2, documents = out2$documents), "Mod14"))
M15ExSem_2<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15_2), semanticCoherence(model=abstractfit_k15_2, documents = out2$documents), "Mod15"))

ModsExSem_2<-rbind(M9ExSem_2,M10ExSem_2, M11ExSem_2, M12ExSem_2, M13ExSem_2, M14ExSem_2, M15ExSem_2)
colnames(ModsExSem_2)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_2$Exclusivity<-as.numeric(as.character(ModsExSem_2$Exclusivity))
ModsExSem_2$SemanticCoherence<-as.numeric(as.character(ModsExSem_2$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_2, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer ## 9 10 11

# zoom in on model 11 12 13
ModsExSem_2 %>% filter(Model %in% c("Mod11", "Mod12","Mod13")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 12 is the best
# evaluation metrics: 
# 12 9.147736 -80.38584 -7.302575 1.493823

##----------------------------------------------------------------------------------------------------
## 4) structured topic model: time interval covariate, no stemming, no stopwords removal
##----------------------------------------------------------------------------------------------------

#prepare dtm

processed3 <- textProcessor(tb.truedatabase$abstract, metadata=tb.truedatabase, stem = FALSE, removestopwords = FALSE)
out3 <- prepDocuments(processed3$documents, processed3$vocab, processed3$meta)
docs3 <- out3$documents
vocab3 <- out3$vocab #-> original words
meta3 <- out3$meta #-> original dataset, used for getting the covariate


# creating a lot of different topic models with different number of topics, including the time covariate
k_seq <- seq(3,30)
storage3 <- searchK(docs3, vocab3, K=k_seq, 
                    prevalence= ~ time, data=meta3, cores = 2)

# evaluate the models by held-out likelihood, semantic coherence, residuals & lower bound
print(storage3)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage3) # --> looking at the residuals & semantic coherence (elbow), the optimal K is between 11 and 15


# we fit the models with K between 9 and 15 

abstractfit_k9_3 <- stm(documents = docs3, vocab=vocab3,
                        K=9, prevalence=~time, data=meta3, init.type="Spectral")

abstractfit_k10_3 <- stm(documents = docs3, vocab=vocab3,
                         K=10, prevalence=~time, data=meta3, init.type="Spectral")

abstractfit_k11_3 <- stm(documents = docs3, vocab=vocab3,
                         K=11, prevalence=~time, data=meta3, init.type="Spectral")

abstractfit_k12_3 <- stm(documents = docs3, vocab=vocab3,
                         K=12, prevalence=~time, data=meta3, init.type="Spectral")

abstractfit_k13_3 <- stm(documents = docs3, vocab=vocab3,
                         K=13, prevalence=~time, data=meta3, init.type="Spectral")

abstractfit_k14_3 <- stm(documents = docs3, vocab3,
                         K=14, prevalence=~time, data=meta3, init.type="Spectral")

abstractfit_k15_3 <- stm(documents = docs3, vocab=vocab3,
                         K=15, prevalence=~time, data=meta3, init.type="Spectral")

# we zoom in on the trade off between exclusivity and semantic coherence for these models
# we want the model that has the highest exclusiveness and coherence at the same time

M9ExSem_3<-as.data.frame(cbind(c(1:9),exclusivity(abstractfit_k9_3), semanticCoherence(model=abstractfit_k9_3, documents = docs3), "Mod09"))
M10ExSem_3<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_3), semanticCoherence(model=abstractfit_k10_3, documents = docs3), "Mod10"))
M11ExSem_3<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_3), semanticCoherence(model=abstractfit_k11_3, documents = docs3), "Mod11"))
M12ExSem_3<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_3), semanticCoherence(model=abstractfit_k12_3, documents = docs3), "Mod12"))
M13ExSem_3<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_3), semanticCoherence(model=abstractfit_k13_3, documents = docs3), "Mod13"))
M14ExSem_3<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_3), semanticCoherence(model=abstractfit_k14_3, documents = docs3), "Mod14"))
M15ExSem_3<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15_3), semanticCoherence(model=abstractfit_k15_3, documents = docs3), "Mod15"))

ModsExSem_3<-rbind(M9ExSem_3,M10ExSem_3, M11ExSem_3, M12ExSem_3, M13ExSem_3, M14ExSem_3, M15ExSem_3)
colnames(ModsExSem_3)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_3$Exclusivity<-as.numeric(as.character(ModsExSem_3$Exclusivity))
ModsExSem_3$SemanticCoherence<-as.numeric(as.character(ModsExSem_3$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_3, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") +theme_bw()

plotexcoer # --> model 9

# zoom in on model 9 11
ModsExSem_3 %>% filter(Model %in% c("Mod09","Mod12","Mod13","Mod15")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 9 is the best
# evaluation metrics: 
#9  8.44126 -47.86885 -6.726469 1.521359

##----------------------------------------------------------------------------------------------------
## 5) structured topic model:  time interval covariate, with stemming, no stopwords removal
##----------------------------------------------------------------------------------------------------
#prepare dtm

processed4 <- textProcessor(tb.truedatabase$abstract, metadata=tb.truedatabase, stem = TRUE, removestopwords = FALSE)
out4 <- prepDocuments(processed4$documents, processed4$vocab, processed4$meta)
docs4 <- out4$documents
vocab4 <- out4$vocab #-> original words
meta4 <- out4$meta #-> original dataset, used for getting the covariate


# creating a lot of different topic models with different number of topics, including the time covariate
k_seq <- seq(3,30)
storage4 <- searchK(docs4, vocab4, K=k_seq, 
                    prevalence= ~ time, data=meta4, cores = 2)

# evaluate the models by held-out likelihood, semantic coherence, residuals & lower bound
print(storage4)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage4) # --> looking at the residuals & semantic coherence (elbow), the optimal K is between 10 and 15


# we fit the models with K between 9 and 15 

abstractfit_k9_4 <- stm(documents = docs4, vocab=vocab4,
                        K=9, prevalence=~time, data=meta4, init.type="Spectral")

abstractfit_k10_4 <- stm(documents = docs4, vocab=vocab4,
                         K=10, prevalence=~time, data=meta4, init.type="Spectral")

abstractfit_k11_4 <- stm(documents = docs4, vocab=vocab4,
                         K=11, prevalence=~time, data=meta4, init.type="Spectral")

abstractfit_k12_4 <- stm(documents = docs4, vocab=vocab4,
                         K=12, prevalence=~time, data=meta4, init.type="Spectral")

abstractfit_k13_4 <- stm(documents = docs4, vocab=vocab4,
                         K=13, prevalence=~time, data=meta4, init.type="Spectral")

abstractfit_k14_4 <- stm(documents = docs4, vocab4,
                         K=14, prevalence=~time, data=meta4, init.type="Spectral")

abstractfit_k15_4 <- stm(documents = docs4, vocab=vocab4,
                         K=15, prevalence=~time, data=meta4, init.type="Spectral")

# we zoom in on the trade off between exclusivity and semantic coherence for these models
# we want the model that has the highest exclusiveness and coherence at the same time

M9ExSem_4<-as.data.frame(cbind(c(1:9),exclusivity(abstractfit_k9_4), semanticCoherence(model=abstractfit_k9_4, documents = docs4), "Mod09"))
M10ExSem_4<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_4), semanticCoherence(model=abstractfit_k10_4, documents = docs4), "Mod10"))
M11ExSem_4<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_4), semanticCoherence(model=abstractfit_k11_4, documents = docs4), "Mod11"))
M12ExSem_4<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_4), semanticCoherence(model=abstractfit_k12_4, documents = docs4), "Mod12"))
M13ExSem_4<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_4), semanticCoherence(model=abstractfit_k13_4, documents = docs4), "Mod13"))
M14ExSem_4<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_4), semanticCoherence(model=abstractfit_k14_4, documents = docs4), "Mod14"))
M15ExSem_4<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15_4), semanticCoherence(model=abstractfit_k15_4, documents = docs4), "Mod15"))

ModsExSem_4<-rbind(M9ExSem_4, M10ExSem_4, M11ExSem_4, M12ExSem_4, M13ExSem_4, M14ExSem_4, M15ExSem_4)
colnames(ModsExSem_4)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_4$Exclusivity<-as.numeric(as.character(ModsExSem_4$Exclusivity))
ModsExSem_4$SemanticCoherence<-as.numeric(as.character(ModsExSem_4$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_4, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer # --> model 10 11 12 13

# zoom in on model 11 12
ModsExSem_4 %>% filter(Model %in% c("Mod11","Mod12")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 11 is the best
# evaluation metrics: 
#11 8.753857 -54.99371 -6.172882 1.517559


##----------------------------------------------------------------------------------------------------
## 6) structured topic model: continuous time covariate, with stemming, stopword removal
##----------------------------------------------------------------------------------------------------

# use same dtm as in 2)

# use cal_year as covariate
attach(tb.truedatabase)
cal_year <- as.numeric(cal_year)

# creating a lot of different topic models with different number of topics, including the time covariate

k_seq <- seq(3,30)
storage5 <- searchK(docs, vocab, K=k_seq, 
                    prevalence= ~ s(cal_year), data=meta, cores = 2)

# evaluate the models by held-out likelihood, semantic coherence, residuals & lower bound
print(storage5)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage5) # --> looking at the residuals & semantic coherence (elbow), the optimal K is between 10 and 15


# we fit the models with K between 11 and 15 

abstractfit_k10_5 <- stm(documents = docs, vocab=vocab,
                         K=10, prevalence=~s(cal_year), data=meta, init.type="Spectral")

abstractfit_k11_5 <- stm(documents = docs, vocab=vocab,
                         K=11, prevalence=~s(cal_year), data=meta, init.type="Spectral")

abstractfit_k12_5 <- stm(documents = docs, vocab=vocab,
                         K=12, prevalence=~s(cal_year), data=meta, init.type="Spectral")

abstractfit_k13_5 <- stm(documents = docs, vocab=vocab,
                         K=13, prevalence=~s(cal_year), data=meta, init.type="Spectral")

abstractfit_k14_5 <- stm(documents = docs, vocab=vocab,
                         K=14, prevalence=~s(cal_year), data=meta, init.type="Spectral")

abstractfit_k15_5 <- stm(documents = docs, vocab=vocab,
                         K=15, prevalence=~s(cal_year), data=meta, init.type="Spectral")

# we zoom in on the trade off between exclusivity and semantic coherence for these models
# we want the model that has the highest exclusiveness and coherence at the same time

M10ExSem_5<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_5), semanticCoherence(model=abstractfit_k10_5, documents = docs), "Mod10"))
M11ExSem_5<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_5), semanticCoherence(model=abstractfit_k11_5, documents = docs), "Mod11"))
M12ExSem_5<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_5), semanticCoherence(model=abstractfit_k12_5, documents = docs), "Mod12"))
M13ExSem_5<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_5), semanticCoherence(model=abstractfit_k13_5, documents = docs), "Mod13"))
M14ExSem_5<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_5), semanticCoherence(model=abstractfit_k14_5, documents = docs), "Mod14"))
M15ExSem_5<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15_5), semanticCoherence(model=abstractfit_k15_5, documents = docs), "Mod15"))

ModsExSem_5<-rbind(M10ExSem_5, M11ExSem_5, M12ExSem_5, M13ExSem_5, M14ExSem_5, M15ExSem_5)
colnames(ModsExSem_5)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_5$Exclusivity<-as.numeric(as.character(ModsExSem_5$Exclusivity))
ModsExSem_5$SemanticCoherence<-as.numeric(as.character(ModsExSem_5$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_5, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer # --> model 10

# zoom in on model 10 and 11 12
ModsExSem_5 %>% filter(Model %in% c("Mod11","Mod12","Mod13")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 10 is the best
# evaluation metrics: 
# 13 9.183148 -71.28131 -6.737334  1.53752

##----------------------------------------------------------------------------------------------------
## 7) structured topic model: continuous time covariate, without stemming, stopwords removal
##----------------------------------------------------------------------------------------------------

#use dtm as in 3)

# creating a lot of different topic models with different number of topics, including the time covariate

k_seq <- seq(3,30)
storage6 <- searchK(out2$documents, out2$vocab, K=k_seq, 
                    prevalence= ~ s(cal_year), data=meta2, cores = 2)
print(storage6)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage6) #clearly in between 7 and 14 based on residuals and semantic coherence

# we fit the models with K between 7 and 14

abstractfit_k7_6 <- stm(documents = docs2, vocab=vocab2,
                        K=7, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k8_6 <- stm(documents = docs2, vocab=vocab2,
                        K=8, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k9_6 <- stm(documents = docs2, vocab=vocab2,
                        K=9, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k10_6 <- stm(documents = docs2, vocab=vocab2,
                         K=10, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k11_6 <- stm(documents = docs2, vocab=vocab2,
                         K=11, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k12_6 <- stm(documents = docs2, vocab=vocab2,
                         K=12, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k13_6 <- stm(documents = docs2, vocab=vocab2,
                         K=13, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

abstractfit_k14_6 <- stm(documents = docs2, vocab=vocab2,
                         K=14, prevalence=~s(cal_year), data=meta2, init.type="Spectral")

M7ExSem_6<-as.data.frame(cbind(c(1:7),exclusivity(abstractfit_k7_6), semanticCoherence(model=abstractfit_k7_6, documents = out2$documents), "Mod07"))
M8ExSem_6<-as.data.frame(cbind(c(1:8),exclusivity(abstractfit_k8_6), semanticCoherence(model=abstractfit_k8_6, documents = out2$documents), "Mod08"))
M9ExSem_6<-as.data.frame(cbind(c(1:9),exclusivity(abstractfit_k9_6), semanticCoherence(model=abstractfit_k9_6, documents = out2$documents), "Mod09"))
M10ExSem_6<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_6), semanticCoherence(model=abstractfit_k10_6, documents = out2$documents), "Mod10"))
M11ExSem_6<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_6), semanticCoherence(model=abstractfit_k11_6, documents = out2$documents), "Mod11"))
M12ExSem_6<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_6), semanticCoherence(model=abstractfit_k12_6, documents = out2$documents), "Mod12"))
M13ExSem_6<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_6), semanticCoherence(model=abstractfit_k13_6, documents = out2$documents), "Mod13"))
M14ExSem_6<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_6), semanticCoherence(model=abstractfit_k14_6, documents = out2$documents), "Mod14"))

ModsExSem_6<-rbind(M7ExSem_6, M8ExSem_6, M9ExSem_6,M10ExSem_6, M11ExSem_6, M12ExSem_6, M13ExSem_6, M14ExSem_6)
colnames(ModsExSem_6)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_6$Exclusivity<-as.numeric(as.character(ModsExSem_6$Exclusivity))
ModsExSem_6$SemanticCoherence<-as.numeric(as.character(ModsExSem_6$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_6, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer ## 9 10 11 12

# zoom in on model 9 11
ModsExSem_6 %>% filter(Model %in% c("Mod10","Mod12")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 10 is the best
# evaluation metrics: 
#12 9.132185 -81.88438 -7.641965 1.539436

##----------------------------------------------------------------------------------------------------
## 8) structured topic model: continuous time covariate, no stemming, no stopwords removal
##----------------------------------------------------------------------------------------------------

#same dtm as in 4)

# creating a lot of different topic models with different number of topics, including the time covariate
k_seq <- seq(3,30)
storage7 <- searchK(docs3, vocab3, K=k_seq, 
                    prevalence= ~ s(cal_year), data=meta3, cores = 2)

# evaluate the models by held-out likelihood, semantic coherence, residuals & lower bound
print(storage7)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage7) # --> looking at the residuals & semantic coherence (elbow), the optimal K is between 9 and 15


# we fit the models with K between 9 and 15 
abstractfit_k9_7 <- stm(documents = docs3, vocab=vocab3,
                        K=9, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

abstractfit_k10_7 <- stm(documents = docs3, vocab=vocab3,
                         K=10, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

abstractfit_k11_7 <- stm(documents = docs3, vocab=vocab3,
                         K=11, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

abstractfit_k12_7 <- stm(documents = docs3, vocab=vocab3,
                         K=12, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

abstractfit_k13_7 <- stm(documents = docs3, vocab=vocab3,
                         K=13, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

abstractfit_k14_7 <- stm(documents = docs3, vocab3,
                         K=14, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

abstractfit_k15_7 <- stm(documents = docs3, vocab=vocab3,
                         K=15, prevalence=~s(cal_year), data=meta3, init.type="Spectral")

# we zoom in on the trade off between exclusivity and semantic coherence for these models
# we want the model that has the highest exclusiveness and coherence at the same time

M9ExSem_7<-as.data.frame(cbind(c(1:9),exclusivity(abstractfit_k9_7), semanticCoherence(model=abstractfit_k9_7, documents = docs3), "Mod09"))
M10ExSem_7<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_7), semanticCoherence(model=abstractfit_k10_7, documents = docs3), "Mod10"))
M11ExSem_7<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_7), semanticCoherence(model=abstractfit_k11_7, documents = docs3), "Mod11"))
M12ExSem_7<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_7), semanticCoherence(model=abstractfit_k12_7, documents = docs3), "Mod12"))
M13ExSem_7<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_7), semanticCoherence(model=abstractfit_k13_7, documents = docs3), "Mod13"))
M14ExSem_7<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_7), semanticCoherence(model=abstractfit_k14_7, documents = docs3), "Mod14"))
M15ExSem_7<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15_7), semanticCoherence(model=abstractfit_k15_7, documents = docs3), "Mod15"))

ModsExSem_7<-rbind(M9ExSem_7,M10ExSem_7, M11ExSem_7, M12ExSem_7, M13ExSem_7, M14ExSem_7, M15ExSem_7)
colnames(ModsExSem_7)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_7$Exclusivity<-as.numeric(as.character(ModsExSem_7$Exclusivity))
ModsExSem_7$SemanticCoherence<-as.numeric(as.character(ModsExSem_7$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_7, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer # --> model 14

# zoom in on model 9 14 15
ModsExSem_7 %>% filter(Model %in% c("Mod09","Mod15")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

# model 9 is the best
# evaluation metrics: 
# 9 8.391423  -49.1224 -6.723023 1.537909 --> BEST, but not better than interval model

##----------------------------------------------------------------------------------------------------
## 9) structured topic model: continuous time covariate, with stemming, no stopwords removal
##----------------------------------------------------------------------------------------------------
#prepare dtm

# same dtm as in 5)


# creating a lot of different topic models with different number of topics, including the time covariate
k_seq <- seq(3,30)
storage8 <- searchK(docs4, vocab4, K=k_seq, 
                    prevalence= ~ s(cal_year), data=meta4, cores = 2)

# evaluate the models by held-out likelihood, semantic coherence, residuals & lower bound
print(storage8)
options(repr.plot.width=6,
        repr.plot.height=6)
plot(storage8) # --> looking at the residuals & semantic coherence (elbow), the optimal K is between 10 and 15


# we fit the models with K between 10 and 15

abstractfit_k10_8 <- stm(documents = docs4, vocab=vocab4,
                         K=10, prevalence=~s(cal_year), data=meta4, init.type="Spectral")

abstractfit_k11_8 <- stm(documents = docs4, vocab=vocab4,
                         K=11, prevalence=~s(cal_year), data=meta4, init.type="Spectral")

abstractfit_k12_8 <- stm(documents = docs4, vocab=vocab4,
                         K=12, prevalence=~s(cal_year), data=meta4, init.type="Spectral")

abstractfit_k13_8 <- stm(documents = docs4, vocab=vocab4,
                         K=13, prevalence=~s(cal_year), data=meta4, init.type="Spectral")

abstractfit_k14_8 <- stm(documents = docs4, vocab4,
                         K=14, prevalence=~s(cal_year), data=meta4, init.type="Spectral")

abstractfit_k15_8 <- stm(documents = docs4, vocab=vocab4,
                         K=15, prevalence=~s(cal_year), data=meta4, init.type="Spectral")

# we zoom in on the trade off between exclusivity and semantic coherence for these models
# we want the model that has the highest exclusiveness and coherence at the same time

M10ExSem_8<-as.data.frame(cbind(c(1:10),exclusivity(abstractfit_k10_8), semanticCoherence(model=abstractfit_k10_8, documents = docs4), "Mod10"))
M11ExSem_8<-as.data.frame(cbind(c(1:11),exclusivity(abstractfit_k11_8), semanticCoherence(model=abstractfit_k11_8, documents = docs4), "Mod11"))
M12ExSem_8<-as.data.frame(cbind(c(1:12),exclusivity(abstractfit_k12_8), semanticCoherence(model=abstractfit_k12_8, documents = docs4), "Mod12"))
M13ExSem_8<-as.data.frame(cbind(c(1:13),exclusivity(abstractfit_k13_8), semanticCoherence(model=abstractfit_k13_8, documents = docs4), "Mod13"))
M14ExSem_8<-as.data.frame(cbind(c(1:14),exclusivity(abstractfit_k14_8), semanticCoherence(model=abstractfit_k14_8, documents = docs4), "Mod14"))
M15ExSem_8<-as.data.frame(cbind(c(1:15),exclusivity(abstractfit_k15_8), semanticCoherence(model=abstractfit_k15_8, documents = docs4), "Mod15"))

ModsExSem_8<-rbind(M10ExSem_8, M11ExSem_8, M12ExSem_8, M13ExSem_8, M14ExSem_8, M15ExSem_8)
colnames(ModsExSem_8)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem_8$Exclusivity<-as.numeric(as.character(ModsExSem_8$Exclusivity))
ModsExSem_8$SemanticCoherence<-as.numeric(as.character(ModsExSem_8$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer <- ggplot(ModsExSem_8, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") + theme_bw()

plotexcoer # --> model 11 12 13

# zoom in on models
ModsExSem_8 %>% filter(Model %in% c("Mod11","Mod12")) %>% 
  ggplot(aes(SemanticCoherence, Exclusivity, color=Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

#model 11
# evaluation metrics: 
#11 8.658032 -50.76004 -6.227121 1.517953 --> BEST, but not better than interval model


##----------------------------------------------------------------------------------------------------
## 9) analysis
##----------------------------------------------------------------------------------------------------

#chosen model: abstractfit_k9_3

##overview of the topics
labelTopics(abstractfit_k9_3,n=10)
plot(abstractfit_k9_3, type="labels", n=10,width = 150) ##overview of the topics

#word prevalence per topic
td_beta <- tidy(abstractfit_k9_3)

highest_word_probs_per_topic <- td_beta %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics") #not insightful because w/o stopword removal and stemming

#topic prevalence
td_gamma <- tidy(abstractfit_k9_3, matrix="gamma", document_names = row.names(docs3))
gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  ggplot(aes(topic, gamma)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.15),
                     labels = percent_format()) +
  theme_bw() +
  labs(x = NULL, y = expression(gamma))

##topic probability distribution of one document
td_gamma %>% filter(document==68) %>% ggplot(aes(as.character(topic), gamma)) + geom_bar(stat="identity") + theme_bw() + labs(x="topics",y= expression(gamma))

## papers per topic
td_gamma %>% filter(gamma>0.5) %>% ggplot(aes(as.character(topic))) + geom_bar(stat="count") + theme_bw() + labs(x="topics", y="number of articles")

#papers per topic
gamma_topic_1 <- td_gamma %>% filter(topic==1)
gamma_topic_2 <- td_gamma %>% filter(topic==2)
gamma_topic_3 <- td_gamma %>% filter(topic==3)
gamma_topic_4 <- td_gamma %>% filter(topic==4)
gamma_topic_5 <- td_gamma %>% filter(topic==5)
gamma_topic_6 <- td_gamma %>% filter(topic==6)
gamma_topic_7 <- td_gamma %>% filter(topic==7)
gamma_topic_8 <- td_gamma %>% filter(topic==8)
gamma_topic_9 <- td_gamma %>% filter(topic==9)



##how does it connect to the metadata?
out3$meta$time <- as.factor(out3$meta$time)
prep <- estimateEffect(1:9 ~ time, abstractfit_k9_3, meta=out3$meta, uncertainty=("Global"))
summary(prep)
par(mfrow = c(1, 1))
options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
time_plot <- plot(prep, "time", topics=c(1,2,3,4,5,6,7,8,9), method='difference', model=abstractfit_k9_3, cov.value1 = "2020-2022", cov.value2 = "1990-1999",
                  xlab = "Difference in topic prevalence between 1990-1999 and 2020-2022", 
                  xlim=c(-0.7, 0.7), ci.level=0.90, labeltype='custom', custom.labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9"))
plot(prep, "time", topics=1, method="pointestimate", main= "(a) Time effects of Topic 1", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=2, method="pointestimate", main= "(b) Time effects of Topic 2", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=3, method="pointestimate", main= "(c) Time effects of Topic 3", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=4, method="pointestimate", main= "(d) Time effects of Topic 4", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=5, method="pointestimate", main= "(e) Time effects of Topic 5", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=6, method="pointestimate", main= "(f) Time effects of Topic 6", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=7, method="pointestimate", main= "(g) Time effects of Topic 7", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=8, method="pointestimate", main= "(h) Time effects of Topic 8", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))
plot(prep, "time", topics=9, method="pointestimate", main= "(i) Time effects of Topic 9", model=abstractfit_k9_3, xlim=c(-0.5, 0.5))


##how are they correlated?

mod.out.corr <- topicCorr(abstractfit_k9_3, cutoff=0.001)
plot(mod.out.corr) ##no correlation

mod.out.corr.huge <- topicCorr(abstractfit_k9_3, method="huge")
plot(mod.out.corr.huge) ##no correlation

#hierarchically
library(stmCorrViz)
stmCorrViz(abstractfit_k9_3, file_out="topicmodel9.html", 
           out3$meta$abstract, out3$documents, labels_number = 3, display=TRUE, verbose = TRUE) ##Unable because downloads are restricted

save.image(file = "thesis.RData")

library(stminsights)
run_stminsights()



