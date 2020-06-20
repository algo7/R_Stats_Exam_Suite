#!/usr/bin/env Rscript
rm(list=ls())
library(ggplot2);
library(ggpubr);
library(digest);
library(argparser);
library(cli);
library(utils);
library(stats);
library(openintro);


#import the data from the file browser
# data <- read.csv(file.choose(),stringsAsFactors=FALSE);

# Welcome Message
welcomeMsg<-'Hi Welcome to the Stats Terminal'
cli::cat_boxx(welcomeMsg)

# Topic 1
# Main Menu List
menuListT1<-c(
  'Indicators (Location: Avg.,Median,Mode | Variability: Stdev,IQR,Range,CV)',
  'Relationship',
  'Histogram (Continous Data)',
  'Bar (Categorical Data)',
  'Frequency Table',
  'Back'
);

# Main Menu Selection Function
topicI<-function(){
  choice<-menu(menuListT1,title='What do you need?')
  switch (choice,
    '1' = indicators(),
    '2' = relationship(),
    '3' = histo(),
    '4' = bar(),
    '5' = freqTableM(),
    '6' = topicSelect()
  )
}

# Location Indicators Menu List
subMenu<-c(
 'User Input',
 'File',
 'File Summary',
 'Back'
)

# Location Indicators Selection Function
indicators<-function(){
  choice<-menu(subMenu,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = {cat(indicatorsCal(toInt(inpSplit('Enter Values Separated by a Comma: ')),'user'));cat('\n');indicators()},
          '2' = {cat(indicatorsCal(read.csv(file.choose(),stringsAsFactors=FALSE),'csv'));cat('\n');indicators()},
          '3' = {cat(summary(read.csv(file.choose(),stringsAsFactors=FALSE)),sep='\n');cat('\n');indicators()},
          '4' = topicI()
  )
}

# Stats Basic Func.
indicatorsCal<-function(input,type){
 if(identical(type,'user')){
   u <- sd(input,na.rm = TRUE)
   v <- max(input,na.rm = TRUE) - min(input,na.rm = TRUE)
   m <- min(input,na.rm = TRUE)
   mx <- max(input,na.rm = TRUE)
   r <- IQR(input,na.rm = TRUE)
   w <- mean(input, na.rm = TRUE)
   x <- median(input, na.rm = TRUE)
   y <- getmode(input)
   cv <- u/w
   z <- c('Mean:',w,'Median:',x,'Mode:',y,'Stdev:',u,'IQR:',r,'Range:',v,'Min:',m,'Max:',mx,'CV:',cv)
   return(z);
 }else{
   u <- sd(unlist(input),na.rm = TRUE)
   v <- max(unlist(input),na.rm = TRUE) - min(input,na.rm = TRUE)
   m <- min(unlist(input),na.rm = TRUE)
   mx <- max(unlist(input),na.rm = TRUE)
   r <- IQR(unlist(input),na.rm = TRUE)
   w <- mean(unlist(input),na.rm = TRUE)
   x <- median(unlist(input),na.rm = TRUE)
   y <- getmode(unlist(input))
   cv <- u/w
   z <- c('Mean:',w,'Median:',x,'Mode:',y,'Stdev:',u,'IQR:',r,'Range:',v,'Min:',m,'Max:',mx,'CV:',cv)
   return (z)
 }
}

# Relationship Menu List
subMenu1<-c(
  'User Input',
  'File',
  'Back'
)

# Relationship Menu Selection Function
relationship<-function(){
  choice<-menu(subMenu1,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = {cat(relationshipCal(toInt(inpSplit('2-Enter Values Separated by a Comma: ')),toInt(inpSplit('1-Enter Values Separated by a Comma: ')),'user'));cat('\n');relationship()},
          '2' = {cat(relationshipCal(read.csv(file.choose(),stringsAsFactors=FALSE),'x','csv'));cat('\n');relationship()},
          '3' = topicI()
  )
}

# Relationship var. cal. func.
relationshipCal<-function(input,input1,type){
  if(identical(type,'user')){
    covar <- cov(input,input1)
    correl <- cor(input,input1)
    z <- c('Covariance:',covar,'Correlation:',correl)
    return(z)
  }else{
    covar <- cov(input[,1],input[,2])
    correl <- cor(input[,1],input[,2])
    z <- c('Covariance:',covar,'Correlation:',correl)
    return(z)
  }
}

# Histogram Function
histo<-function(){
  x <- read.csv(file.choose())
  cn <-colnames(x)
  un <- unlist(x)
  hn<-hist(un, breaks = 'Sturges',plot = FALSE)
  plot(hn,xlim=c(0,hn$breaks[length(hn$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,hn$counts[1]+round(sum(hn$counts)/length(hn$counts))),xlab = cn,main = paste('Graph of',cn),col = 'blue',border='red',labels = TRUE)
  h<-hist(un, breaks = 'Sturges',plot = FALSE)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE, xlab = cn,ylab='RF',xlim=c(0,h$breaks[length(h$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,100),main = paste('Graph of',cn,'percentage version'),col = 'red',border='blue',labels = TRUE)
}

# Barchart Function
bar<-function(){
  x <- read.csv(file.choose())
  df<-data.frame(x[!is.na(x)])
  lvs <- levels(sort(unlist(x)))
  count <- character()
  for (variable in lvs) {
    filter<-df[colnames(df)[1]]==variable
    count<-c(count,length(df[filter,]))
  }
  xs <- data.frame('Levels'=lvs,'Count'=as.integer(count))
  p <- ggplot(xs, aes(x=Levels, y=Count))+geom_bar(stat='identity')+
    geom_text(aes(label=Count), vjust=-0.3, size=3.5)
  print(p)
}

# FreqTable Menu List
subMenuF<-c(
  'Right Open',
  'Right Closed',
  'Back'
)

# Location Indicators Selection Function
freqTableM<-function(){
  choice<-menu(subMenuF,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = freqTable(FALSE),
          '2' = freqTable(TRUE),
          '3' = topicI()
  )
}
# Frequency Table
freqTable<-function(openSide){
  raw<-read.csv(file.choose())
  summary(raw)
  raw<-data.frame(raw[!is.na(raw)])
  unlistraw<-unlist(raw)
  hist <- hist(unlist(raw),breaks="Sturges", plot=FALSE,include.lowest=TRUE,right=openSide)
  br=hist$breaks
  cf = cbind(cumsum(table(cut(unlistraw,br,right=openSide))))
  rf = cbind(table(cut(unlistraw,br,right=openSide)) /nrow(raw))
  crf = cbind(cumsum(table(cut(unlistraw,br,right=openSide))))/nrow(raw)
  df<-data.frame(bin=rownames(crf),AbsFreq_ni=hist$count, CumuFreq=cf, RelativeFreq_fi=rf, Cumu_RelativeFreq_Fi=crf)
  rownames(df)<-NULL
  print(df)
}

# Topic II
menuListT2<-c(
  'Probability Table',
  'Back'
);

# Main Menu Selection Function
topicII<-function(){
  choice<-menu(menuListT2,title='What do you need?')
  switch (choice,
          '1' = probTable(),
          '2' = topicSelect(),
  )
}

probTable<-function(){
  # Import the file
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex,header = TRUE)
  df<-data.frame(x)
  # Row/col names
  rownames(df)<-df[,1]
  df[-c(1),-c(1)]
  df<-df[,-1]
  
  # Vertical Sum
  verticalTotal<-integer()
  for (coln in colnames(df)) {
    verticalTotal<-c(verticalTotal,sum(df[coln]))
  }
  # Update the df
  df<-rbind(df,verticalTotal)
  # Horizontal Sum
  horizontalTotal<-integer()
  for (rown in rownames(df)) {
    horizontalTotal<-c(horizontalTotal,sum(df[rown,]))
  }
  # Update the df
  df<-cbind(df,horizontalTotal)
  # Get the grand total
  ro<-length(rownames(df))
  co<-length(colnames(df))
  grandSum<-df[ro,co]
  # Calculate the percentage table
  per<-data.frame(df/grandSum)
  # Replace the column names
  colnames(df)[co]<-'Total'
  colnames(per)[co]<-'Total'
  # The Table with the sum
  print(df)
  cat('\n')
  # The percentage table
  print(per)
  cat('\n')
  # Remove the Total column
  x<-per[length(rownames(per)),]
  x[,length(colnames(x))]<-NULL
  # Determin the event type
  i<-0
  filter<-character()
  for (cols in x) {
    i<-i+1
    # filter<-c(filter,cols*per[,length(colnames(per))]==per[,i])
    x<-cols*per[,length(colnames(per))]
    y<-per[,i]
    filter<-c(filter,as.double(x)==as.double(y))
  }
 
  roo<-length(rownames(per))
  coo<-length(colnames(per))-1
  eventTable<-matrix(filter,nrow=roo,ncol =coo)
  cat('Event Table: (True = Independent | False = Dependent)',sep = '\n')
  cat('The Last Column will Always Equal True Because it is the Sum',sep = '\n')
  cat('Note:If there are more than or equal to 2 trues then all is true regardless of the display',sep = '\n')
  print(eventTable)
}

treeG<-function(){
  treeDiag(c("Breakfast?", "Go to class"), 
           c(0.4, 0.11, 0.49), 
           list(c(0.4, 0.36,0.24), 
                c(0.6, 0.3, 0.1), 
                c(0.1, 0.4, 0.5)), 
           c("one", "two", "three"), 
           c("Statistics","English", "Sociology"))
}



# Misc.:

# Split input func
inpSplit<-function(text){
  result<- strsplit(readline(prompt=text),",")
  return(result)
}

# Convert to integer func
toInt<-function(list){
  for (variable in list) {
    int<-as.integer(variable)
    return(int)
  }
}

# Get mode func
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Topic Selection
topicSelect=function(){
  menuList<-c(
    'Descriptive Statisitc',
    'Probabilty',
    'Discrete Variables',
    'Continuous Variables',
    'Sampling Distributions',
    'Confidence Interval',
    'Hypothesis Testing',
    'Simple Linear Regression',
    'Multiple Linear Regression',
    'Chi2 Test'
  );
  
  choice<-menu(menuList, title='Please Select A Topic:');
  # Menu Selection Function
  mSelect<-function(topic){
    switch (topic,
            '1' = topicI(),
            '2' = topicII(),
            '3' = topicIII()
    )
  };
  mSelect(choice);
  
}
topicSelect()

# # Take in arguments
# args <- commandArgs(trailingOnly = TRUE);
# if (length(args) < 1){
#   stop('Missing Arguments\n')
# }

# menu(args[1])
# p <- arg_parser('Stats Terminal')
# # Add a positional argument
# p <- add_argument(p, 'name', help='Name of the person to sort')
# p <- add_argument(p, 'namxe', help='Name of the person to sort')
# p <- add_argument(p, 'namse', help='Name of the person to sort')
# # Add a flag
# p <- add_argument(p, '--debug', help='enable debug mode', flag=TRUE)
# # Add another flag
# p <- add_argument(p, '--short', help='output only the house', flag=TRUE)
# argv <- parse_args(p)


