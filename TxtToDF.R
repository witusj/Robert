
# Read text file (file should be in the same working directory as this script)
con <- file("antibiogram_raw.txt")
txtData <- readLines(con, warn = FALSE, skipNul = TRUE)
close(con)

# Reduce multiple tabs to semicolon
txtData <- gsub("\t{1,}", ";", txtData)

# Clean up
txtData <- gsub("^Page", NA, txtData)
txtData <- gsub("Site:", "", txtData)
txtData <- gsub("^;$", NA, txtData)
txtData <- gsub("^Sensitive.*", "New", txtData)
txtData <- gsub("^;", "", txtData)
txtData <- gsub(";$", "", txtData)
txtData <- gsub("S;R;I", "Data", txtData)
txtData <- na.omit(txtData)

# Function to split vector into list

VectorToList <- function(vec, ind) {
  
n = 1
splitIndex <- which(vec == ind)
myList <- NULL

for (i in splitIndex[-length(splitIndex)]) {
  
  myList[[n]] <- vec[(i+1):(splitIndex[n+1]-1)]
  n <- n+1
}
return(myList)
}

# Function to split vector into data frame
VectorToDF <- function(vec, ind){
  
  d <- which(vec[[1]] == ind)+1
  l <- length(vec[[1]])

  myDf <- data.frame(
    matrix(unlist(
      strsplit(
        vec[[1]][d:l], split = ";", fixed = FALSE, perl = FALSE, useBytes = FALSE
      )
    )
    , ncol = 4, byrow = TRUE
    )
  )
  
  return(myDf)
  
}

# Function to select elements from list
SelectListItem <- function(lst, elnt = 1, lkp) {
  
  itemVec <- sapply(lst, function(x) x[[elnt]][[1]]) # Create vector with all items from selected list element
  myList <- lst[which(itemVec == lkp)] # Use which function on vector to create vector of indexnumbers and select items from original list
  
  return(myList)
}

# Create list of separate vectors
txtList <- VectorToList(txtData, "New") 

# Create list of lists with results in data frame
newList <- NULL
for (m in 1:length(txtList)) {
  
  testresDf <- VectorToDF(txtList[m], "Data")
  colnames(testresDf) <- c("Antibiotics", "S", "R", "I")
  newList[[m]] <- list(bacteria = txtList[[m]][1], source = txtList[[m]][2], result = testresDf)
  
}

head(newList, n=3)

SelectListItem(newList, 1, "Escherichia coli")
