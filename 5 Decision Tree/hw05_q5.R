#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("/Users/jingyiyuan/Desktop/Data Mining/R/FederalistPapers")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(tm)
library(NLP)
library(SnowballC)

##########################################
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.directory = function(dirname){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirname)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # inspect to verify
  # inspect(fp[1])
  # another useful command
  # identical(fp[[1]], fp[["Federalist01.txt"]])
  # now let us iterate through and clean this up using tm functionality
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove all punctuation
  fp = tm_map( fp , removePunctuation);
  # remove stopwords like the, a, and so on.	
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)	
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  dir.create( sprintf('%s_clean',dirname) )
  writeCorpus( fp , sprintf('%s_clean',dirname) )
}

##########################################

preprocess.directory("fp_hamilton_train")
preprocess.directory("fp_hamilton_test")
preprocess.directory("fp_madison_train")
preprocess.directory("fp_madison_test")

##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
  # Store the infiles in a list
  infiles = list();
  # Get a list of filenames in the directory
  filenames = dir(dirname,full.names=TRUE);
  for (i in 1:length(filenames)){
    infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(infiles)
}
##########################################

hamilton.train = read.directory("fp_hamilton_train_clean")
hamilton.test = read.directory("fp_hamilton_test_clean")
madison.train = read.directory("fp_madison_train_clean")
madison.test = read.directory("fp_madison_test_clean")

##########################################
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
  # This returns a dataframe that is sorted by the number of times 
  # a word appears
  
  # List of vectors to one big vetor
  dictionary.full <- unlist(infiles) 
  # Tabulates the full dictionary
  tabulate.dic <- tabulate(factor(dictionary.full)) 
  # Find unique values
  dictionary <- unique(dictionary.full) 
  # Sort them alphabetically
  dictionary <- sort(dictionary)
  dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
  sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}
##########################################

all = list(hamilton.test, hamilton.train, madison.train, madison.test)
dictionary = make.sorted.dictionary.df(all)

##########################################
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
  # This takes the text and dictionary objects from above and outputs a 
  # document term matrix
  num.infiles <- length(infiles);
  num.words <- nrow(dictionary);
  # Instantiate a matrix where rows are documents and columns are words
  dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
  for (i in 1:num.infiles){
    num.words.infile <- length(infiles[[i]]);
    infile.temp <- infiles[[i]];
    for (j in 1:num.words.infile){
      ind <- which(dictionary == infile.temp[j])[[1]];
      # print(sprintf('%s,%s', i , ind))
      dtm[i,ind] <- dtm[i,ind] + 1;
    }
  }
  return(dtm);
}
##########################################

dtm.hamilton.train = make.document.term.matrix(hamilton.train, dictionary)
dtm.hamilton.test = make.document.term.matrix(hamilton.test, dictionary)
dtm.madison.train = make.document.term.matrix(madison.train, dictionary)
dtm.madison.test = make.document.term.matrix(madison.test, dictionary)

##########################################
h_r_test = nrow(dtm.hamilton.test)#16
author_h_test = as.integer(rep(1, h_r_test))
h_r_train = nrow(dtm.hamilton.train)#35
author_h_train = as.integer(rep(1, h_r_train))
m_r_test = nrow(dtm.madison.test)#11
author_m_test = as.integer(rep(0, m_r_test))
m_r_train = nrow(dtm.madison.train)#15
author_m_train = as.integer(rep(0, m_r_train))

train_label = as.factor(c(author_h_train, author_m_train))
train_data = rbind(dtm.hamilton.train, dtm.madison.train)
training = as.data.frame(train_data)
training$y = train_label

test_label = as.factor(c(author_h_test, author_m_test))
test_data = rbind(dtm.hamilton.test, dtm.madison.test)
testing = as.data.frame(test_data)
testing$y = test_label

colnames(training) <- c(as.vector(dictionary$word),"y")
colnames(testing) <- c(as.vector(dictionary$word),"y")

#(a)
library(rpart)
tree.training = rpart(y~., data = training)
plot(tree.training, uniform = TRUE, compress = FALSE, margin = 0.1, main = "Gini splits")
text(tree.training, use.n = TRUE)
train.fit = predict(tree.training, testing, type = "class")

right_a = 0
false_n_a = 0
false_p_a = 0
for (i in 1:length(train.fit)){
  if(i < h_r_test+1){
    if(train.fit[i] == 1){
      right_a = right_a + 1
    }
    else {false_n_a = false_n_a + 1}
  }
  else{
    if(train.fit[i] == 0){
      right_a = right_a + 1
    }
    else {false_p_a = false_p_a + 1}#Madison classified as Hamilton
  }
}
right_a = right_a/length(train.fit)
false_p_a = false_p_a/m_r_test#11
false_n_a = false_n_a/h_r_test#16

#(b)
tree.training = rpart(y~., data = training, parms = list(split = 'information'))
plot(tree.training, uniform = TRUE, compress = FALSE, margin = 0.1, main = "Information splits")
text(tree.training, use.n = TRUE)
train.fit = predict(tree.training, testing, type = "class")

right_b = 0
false_n_b = 0
false_p_b = 0
for (i in 1:length(train.fit)){
  if(i < h_r_test+1){
    if(train.fit[i] == 1){
      right_b = right_b + 1
    }
    else {false_n_b = false_n_b + 1}
  }
  else{
    if(train.fit[i] == 0){
      right_b = right_b + 1
    }
    else {false_p_b = false_p_b + 1}#Madison classified as Hamilton
  }
}
right_b = right_b/length(train.fit)
false_p_b = false_p_b/m_r_test
false_n_b = false_n_b/h_r_test
  