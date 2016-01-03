analytics.accounts = function (accessToken) {
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

analytics.account.profiles = function(account, context = FALSE) {
  profileList = list( account = account$name, profiles = data.frame()  );
  profileIndex = 1;

  for(p in account[['profiles']]) {
    for (pp in p) { 
      profileListItem = data.frame(name = pp$name, id = pp$id);
      
      profileList$profiles = rbind(profileList$profiles, profileListItem);
    }
  }

  if(context == TRUE) {
    return(profileList);
  } else {
    return(profileList$profiles);
  }
}

parseAccounts = function(accts) {
  acctsList = data.frame();

  for(acct in accts) {
    acctList = do.call(rbind, lapply(acct$webProperties, function(x) x));
    acctList = cbind(name = acct$name, accountID = acct$id, acctList);
    acctsList = rbind( acctList, acctsList );
  }
  #list.apply(acctsList[,c("profiles")], function(l){
  #  print(l[[1]])
  #})
  
  return(acctsList)
}