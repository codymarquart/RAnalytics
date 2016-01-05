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
  
  accts = parseAccounts(content(req)$items);
  #print((accts[,9]))
  #list.iter(accts, print(.))
  for(a in accts) {
    #print(a[9])
    #for(p in a[9]) {
     #print(p);
    #}
  }

  return(accts);
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
  
  acctsTable = as.data.table(t(as.data.table(accts)));
  colnames(acctsTable) = names(accts[[1]]);
    
  for(acct in accts) {
    acctList = do.call(rbind, lapply(acct$webProperties, function(x) {
      p = list.stack(x$profiles);
      rownames(p) <- list.names(x$profiles[[1]][[1]]);
      pDF = as.data.table(p);
      colnames(pDF) = colnames(p);
      x$profiles = as.data.table(t(as.data.table(t(p))));
      colnames(x$profiles) = colnames(p);
      return(x);
    }));
    
    acctList = cbind(name = acct$name, accountID = acct$id, acctList);
    acctsList = rbind( as.data.table(acctList), acctsList );
  }
  
  return(acctsList)
}