# these are the libraries that need to be installed
library(stm)
library(tm)
library(SnowballC)

# seed the random number generator for reproducibility
set.seed(827)

# the object `posts` is a data frame containing text data, 
# the object `enrol` is a data frame containing enrolment
# data (anonymised data relating to participants),
# including informed consent status
posts$X1 <- posts$text #  - retain original text, column X1 is for analysis only

# ensure that only posts from consenting participants can be seen by researchers
posts$X2 <- NA
consenting <- enrol$learning_management_system_user_id[enrol$research_consent == 1]
posts$X2[posts$PostingUserId %in% consenting] <- posts$text[posts$PostingUserId %in% consenting] #  - X2 is for pulling out exemplars later (recycling code) so it is only populated where consent has been given

# categorise each post by participant characteristics
posts$cat <- "catNA"   # some posters did not provide enough information to be categorised
posts$cat[posts$PostingUserId %in% cat1$learning_management_system_user_id] <- "cat1"
posts$cat[posts$PostingUserId %in% cat2$learning_management_system_user_id] <- "cat2"
posts$cat[posts$PostingUserId %in% cat3$learning_management_system_user_id] <- "cat3"
posts$cat[posts$PostingUserId %in% cat4$learning_management_system_user_id] <- "cat4"


# pre-processing:  (much of this is only necessary due to data collection constraints)
posts$X1 <- gsub("00A0", "", posts$X1)
posts$X1 <- gsub("0092", "", posts$X1)
posts$X1 <- gsub("NA", "", posts$X1)
posts$X1 <- gsub("'", "", posts$X1)  # remove apostrophes
posts$X1 <- gsub("[[:punct:]]", " ", posts$X1)  # replace punctuation with space
posts$X1 <- gsub("[[:cntrl:]]", " ", posts$X1)  # replace control characters with space
posts$X1 <- gsub("^[[:space:]]+", "", posts$X1) # remove whitespace at beginning of documents
posts$X1 <- gsub("[[:space:]]+$", "", posts$X1) # remove whitespace at end of documents
posts$X1 <- tolower(posts$X1)  # force to lowercase


# define stop words
rm <- c("palliation", "palliative")

# K is the number of topics to fit the model to, several fits were attempted with different K
K <- 12

processed <- textProcessor(posts$X1, metadata = posts, onlycharacter = T,
                           striphtml = T, customstopwords = rm, verbose = F)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# fit a prevalence model with `cat` as a covariate/factor
fit <- stm(out$documents, out$vocab, K = K,
           prevalence =~ cat, max.em.its = 150,
           data = out$meta, init.type = "Spectral", verbose = F)


# produce topic labels from the model
labelTopics(fit, c(1:K))

# find exemplars (only from consenting posters) and produce a table including anonymous user id
z <- findThoughts(fit, texts = as.character(out$meta$X2),
                  n = 25)
reflections <- lapply(z$index, function(x) data.frame(meta$PostingUserId[x], meta$X2[x]))
print(reflections)


# plot topic proportions over the entire corpus
topic.labels = c("1. dignity, comfort", "2. planning for eol", "3. QoL", "4. diverse places of care",
                 "5. connection, family", "6. environments", "7. omitted", "8. family support",
                 "9. any life-limiting condition", "10. QoD", "11. maximising function", "12. omitted")
custom.labels = c(rep("", 12))

plot.STM(fit, type = "summary", topics = c(1:K),
         main = "Topic proportions over entire corpus",
         custom.labels = custom.labels, topic.names = topic.labels)



# estimate topic proportions ~ cat
fx <- estimateEffect(c(1:K) ~ cat, fit, meta = out$meta)

# extract estimates and plot proportions for each cat, excluding topics that were not informative
m <- plot.estimateEffect(fx, covariate = "cat", topics = c(1:K),
                               model = fit, method = "pointestimate")
a <- NULL
for(i in c(1:6, 8:11)){
  for(j in seq_along(m$uvals)){
    a <- c(a, m$uvals[j], i, round(m$means[[i]][j], 2), round(m$cis[[i]][1,j], 3), round(m$cis[[i]][2,j], 3))
  }
}

b <- as.data.frame(matrix(a, ncol = 5, byrow = T))
colnames(b) <- c("cat", "topic", "mean", "2.5%CI", "97.5%CI")

for(i in c(5,4,2,3,1)) {
  k <- filter(b, cat == i)
  bp <- ggplot(k, aes(x = topic, y = mean)) +
    geom_errorbar(aes(ymin = mean - `2.5%CI`, ymax = mean + `97.5%CI`), width = 0.1) +
    geom_point() + scale_x_continuous(breaks = c(1:K), labels = c(1:K)) +
    ggtitle(paste0("Cohort ", i)) + scale_y_continuous(limits = c(0,0.42)) + coord_flip()
  print(bp)
  
}



# plot all pairwise comparisons,  Pr(topic i | cat = j) - Pr(topic i | cat = k)
plot.estimateEffect(fx, covariate = "cat", topics = c(1:6, 8:11),
                    model = fit, method = "difference", cov.value1 = "cat1", cov.value2 = "cat2")

plot.estimateEffect(fx, covariate = "cat", topics = c(1:6, 8:11),
                    model = fit, method = "difference", cov.value1 = "cat1", cov.value2 = "cat3")

plot.estimateEffect(fx, covariate = "cat", topics = c(1:6, 8:11),
                    model = fit, method = "difference", cov.value1 = "cat1", cov.value2 = "cat4")

plot.estimateEffect(fx, covariate = "cat", topics = c(1:6, 8:11),
                    model = fit, method = "difference", cov.value1 = "cat2", cov.value2 = "cat3")

plot.estimateEffect(fx, covariate = "cat", topics = c(1:6, 8:11),
                    model = fit, method = "difference", cov.value1 = "cat2", cov.value2 = "cat4")

plot.estimateEffect(fx, covariate = "cat", topics = c(1:6, 8:11),
                    model = fit, method = "difference", cov.value1 = "cat3", cov.value2 = "cat4")


