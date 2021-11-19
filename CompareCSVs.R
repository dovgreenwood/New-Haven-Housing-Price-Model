library(dplyr)

#tanvi <- read.csv('data/tanvi.csv')
linda <- read.csv('data/lm934_housing_extracted.csv')
dov <- read.csv('data/dig27_housing_extracted.csv')

#print(paste('Tanvi PIDs: ', length(tanvi$pid)))
print(paste('Linda PIDs: ', length(linda$pid)))
print(paste('Dov PIDs: ', length(dov$pid)))

#### This should be used once we have all of our complete CSVs. ####
#### These lines in particular get the common PIDs, in case there's a mismatch ####
#### between the number of rows in our data. ####
#### Then, we should use these PIDs to compare our data tables, rather than the row indices.  ####
#joint_data <- rbind(tanvi, linda, dov)
#count <- as.vector( table(joint_data$pid) )
#same_pids <- count[count[1] == 3]
#print(paste('Shared PIDs: ', length(same_pids)))

# Note that when we add Tanvi's data in, we should get rid of 'nghd' from here, since
# we already know those are very different (zip code vs neighborhood name).
# I've already said this, but I vote we go with zip code, which is probably more reliable, since
# there won't be spelling errors
to_compare <- c('address', 'value', 'year', 'land', 'living', 'good', 'style', 'grade', 
                'beds', 'heatfuel', 'heattype', 'ac', 'nghd')

diff_rows <- data.frame(matrix(ncol = length(to_compare), nrow = 0))
names(diff_rows) <- to_compare

diff_lbl <- vector()
x = 1

for(i in 1:length(linda$pid)) {
  lrow <- linda[i,]
  drow <- dov[i,]
  
  for(lbl in to_compare) {
    # Trimming makes sure that we don't have any errors resulting from excess whitespace.
    lval <- trimws(lrow[lbl][[1]])
    dval <- trimws(drow[lbl][[1]])
    
    # This is a sanity check to see real-time if there are any mismatches.
    print(lval)
    print(dval)
    print('')
    
    if(lval != dval) {
      diff_rows <- rbind(diff_rows, drow)  # drow versus lrow or trow is an arbitrary choice
      
      diff_lbl[x] <- lbl  # Stores the label where the mismatch can be found
      x = x + 1
      
      break
    }
  }
}

# This will tell us right off the bat how many mismatches there are.
print(x - 1)

# Once the script is run, check the data manually in the console.