library(openintro);
# Total Top Levels
treeTotal<-toInt(readline(prompt='Total Levels: '))
treeTotal<-integer(treeTotal)
# Top Level Names
treeVN<-character()
# Sub Level Names
treeVNsubN<-character()
# Values
treeVal<-integer()

# Loop1
for (variable in 1:length(treeTotal)) {
  # Total Top Level Names
  treeVN<-c(treeVN,readline(prompt=paste('Top Level',variable,'Name',':')))
  # Total Top Level Sub Levels
  treeVNsub<-toInt(readline(prompt=paste('Top Level',variable,'Levels',':')))
  treeVNsub<-integer(treeVNsub)
  for (variable in 1:length(treeVNsub)) {
    treeVNsubN<-c(treeVNsubN,readline(prompt=paste('Sub Level',variable,'Level Name',':')))
    treeVal<-c(treeVal,readline(prompt=paste('Sub Level',variable,'Value',':')))
  }
}
print(treeVN)
print(treeVNsubN)
print(as.numeric(treeVal))
spx<-split(as.numeric(treeVal), sort(as.numeric(treeVal)%%2))
treeDiag(treeVN, c(.4,.6),
         list(c(.36,.34),c(.3,.1)),list(treeVNsubN[1],treeVNsubN[2]),
         list(treeVNsubN[3],treeVNsubN[4]), showWork=TRUE)

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
