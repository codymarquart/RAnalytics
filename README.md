# RAnalytics

###
# Step 1: Authorize the connection to the API
###
ga = GA("./data/testing.json")

###
# Step 2: View accounts to find the correct one to query
# -> Accounts will vary by user
###
View(ga$accounts);

###
# Step 3: Obtain the custom dimensions 
# -> Using ga$account[12] for this example
###
gaDims = ga$getDimensions(accountID = ga$accounts[12]$accountID, webID = ga$accounts[12]$id)

###
# Step 4: Run the query
# -> Again using ga$account[12]
#
# By default, ga$query pulls the data from the last 30 days
#
###
data = ga$query(
  accountID = ga$accounts[12]$accountID, 
  webID = ga$accounts[12]$id, 
  ids = ga$accounts[12]$profiles[[1]][1]$id, 
  uniqueBy = levels(droplevels(gaDims[c(2,8),]$id)), 
  columnNames = gaDims
)

###
# Step 5: View the results
###
View(data$merged)