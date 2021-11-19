library(dplyr)

#' ---
#' title: "Extract New Haven Housing Data"
#' date: "Starting September 8, 2021"
#' author: "Jay Emerson"
#' output: pdf_document
#' # Or use output: html_document
#' # Or don't use either, in which case you will be prompted to choose
#' ---
#' 
#' # Abstract
#' 
#' This script extracts and partially cleans data from the raw New Haven
#' housing HTML files in the specified raw data directory.  The property
#' ID (`pid`) can be obtained from the filename, and we start by gathering
#' the total assessed value information.
#'

# Options to be tweaked can go here:
rawdir <- "rawdata"                    # Can be changed to full data directory
#verbose <- TRUE                        # For optional debugging output
verbose <- FALSE

###########################----CONSTANTS----###############################

cols <- c('pid', 'address', 'value', 'buildings', 'year', 
          'land', 'living', 'good', 'style', 'model', 'grade', 
          'beds', 'baths', 'heatfuel', 'heattype', 'ac', 'nghd') 

# Contains the unique ID's corresponding to the field of the same index in "cols."
# The empty string is for "PID," which is not extracted from a field, but rather the file name.
# The 'BI' strings indicate that the field must be located using the "building.info" array and the
# "getBuildingInfo" function.
# (All "01"s may need to be replaced with specific building number if >1 building...)
labels <- c('', 'MainContent_lblTab1Title', '', 'MainContent_lblBldCount',
            'MainContent_ctl01_lblYearBuilt', 'MainContent_lblLndAcres',
            'MainContent_ctl01_lblBldArea', 'MainContent_ctl01_lblPctGood',
            'BI', 'BI', 'BI', 'BI', 'BI', 'BI', 'BI', 'BI', 
            'MainContent_lblNbhd')

# Regex is necessary because the labels in the "Building Info" table are not always consistent
# Empty strings are those that are to be found in the "labels" array instead.
building.info <- list('', '', '', '', '', '', '', '',
                      '<td>Style:*</td>', 
                      '<td>Model:*</td>', 
                      '<td>Grade:*</td>', 
                      '<td>(Total|Ttl) (Bedrooms|Bedrms|Beds):*</td>', 
                      '<td>(Total|Ttl) (Half)* (Bthrms|Bathrms|Bathrooms|Baths):*</td>', 
                      '<td>Heat Fuel:*</td>', 
                      '<td>Heat Type:*</td>', 
                      '<td>AC Type:*</td>')

# The types of each field (for EVERY entry in cols)--an array of the same length as all previous.
# This is eventually passed to "forceType."
types <- c('numeric', 'character', 'numeric', 'numeric', 'numeric', 'numeric',
           'numeric', 'numeric', 'character', 'character', 'character', 
           'numeric', 'numeric', 'character', 'character', 'character', 
           'nghd') # Neighborhood gets a special identifier to remove alphabetical characters but keep ALL numbers,
                   # i.e. including leading zeroes.


###########################----FUNCTIONS----###############################

# The original code to get the value of a property, moved into a function
# to clean the Main code a bit.
getValue <- function(property) {
  val <- grep('MainContent_lblGenAssessment\">', property, value = TRUE)
  
  if (length(val) == 1) {
    val <- gsub("<[^<>]*>", "", val)
    val <- as.numeric(gsub("[$]|,", "", val))
  } else {
    if (verbose) {
      cat("Assessed value issue, i =", i, "for file", files[i], "\n")
      print(val)
    }
    val <- NA
  }
  
  return(val)
}

# This block transforms the data that has been found into the correct type, and is called
# in the field-getter functions. For numeric fields, it strips non-numeric characters to ensure
# the script will not crash.
forceType <- function(data, dtype) {
  if(dtype == 'numeric') {
    data <- gsub('[^0-9\\.]', '', data)
    return(as.numeric(data))
  }
  else if(dtype == 'nghd') {
    data <- gsub('Neighborhood', '', data, fixed = TRUE)
    # The use of the paste function here ensures that Excel doesn't force the data type to be numeric, which
    # results in eliminating leading zeroes.
    return(paste('\t', data))
  }
  else {
    data <- as.character(data)
    return(data)
    # return(alphabetize(data))
  }
}

# alphabetize <- function(st) {
#   split <- strsplit(st, '/')
#   split <- split[split != '/']
#   in.order <- split[order(split)]
#   return(paste(in.order, collapse = '/'))
# }

# This function is used for fields that are demarcated in the HTML by unique HTML ID's. These ID's (the parameter, "label")
# can be found abovein the "labels" array.
getField <- function(property, label, dtype) {
  data <- grep(label, property, value = TRUE)

  if (length(data) == 1) {
    data <- gsub("<[^<>]*>", "", data[[1]])
    data <- trimws(data)
    data <- forceType(data, dtype)
  }
  else {
    if (verbose) {
      cat("Assessed value issue, i =", i, "for file", files[i], "\n")
      print(data)
    }
    data <- NA
  }
  
  return(data)
}

# This function is called for fields in the "Building Info" table of the page,
# which aren't marked by unique HTML ID's, but rather by text labels. The text labels can be found above in the
# "building.info" array.
getBuildingInfo <- function(property, label, dtype) {
  index <- grep(label, property, value = FALSE)
  data <- gsub(label, '', property[index])
  
  data <- gsub("<[^<>]*>", "", data[1])
  data <- trimws(data)

  if(is.na(data) | nchar(data) == 0) {
    if (verbose) {
      cat("Assessed value issue, i =", i, "for file", files[i], "\n")
      print(data)
    }
    data <- NA
  }
  else
    data <- forceType(data, dtype)
  
  return(data)
}


##############################----MAIN----#################################

files <- dir(rawdir, full.names = TRUE)
pids <- gsub("[^0-9]", "", files)
pids <- as.numeric(pids)

# Initialiall.datae our data.frame:
all.data <- data.frame(matrix(NA,
                       nrow = length(files),                        
                       ncol = length(cols))) 
names(all.data) <- cols
all.data$pid <- pids

# Be careful if length(files) is large -- it's best to debug on small
# samples of files until you're pretty confident in your code.

for (i in 1:length(files)) {
  property <- readLines(files[i]) 
  
  print(paste0(i, '/', length(files)))
  
  # Start by collecting the assessed value of the property
  # (I did not change this as it would have taken unnecessary extra effort, as this already works fine.
  #  "If it ain't broke, don't fix it.")
  val <- getValue(property)
  all.data$value[i] <- val
  
  # Get the rest of the data
  for(j in 1:length(cols)) {
    label = labels[[j]]

    if(label == 'BI')
      data <- getBuildingInfo(property, building.info[[j]], types[[j]])
    else if(label != '')
      data <- getField(property, label, types[[j]])
    else
      next
    all.data[cols[j]][i,] <- data
  }
}

filtered <- filter(all.data, all.data$model == 'Single Family')
filtered <- filter(filtered, filtered$buildings == 1)
ordered <- filtered[order(filtered$pid),]

# Save as a CSV as requested (please change NETID below):
write.csv(ordered, "data/dig27_housing_extracted.csv", row.names = FALSE)


