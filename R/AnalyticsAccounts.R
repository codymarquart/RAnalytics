analytics.accounts = function (accessToken) {
  print(paste("Access: ", accessToken))
  query <- paste(
    "https://content.googleapis.com/analytics/v3/management/accountSummaries", 
    sep = ""
  );
  queryList = list(
    'access_token'= accessToken
  );
  
  req <- GET(query, query = queryList);
  stop_for_status(req);
  
  return(parseAccounts(content(req)$items));
}

parseAccounts = function(accts) {
  acctsList = data.frame();
  
  for(acct in accts) {
    acctList = do.call(rbind, lapply(acct$webProperties, function(x) x));
    acctList = cbind(name = acct$name, id = acct$id, acctList);
    
    acctsList = rbind( acctList, acctsList );
  }
  
  return(acctsList)
}