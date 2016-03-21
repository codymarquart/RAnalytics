#' @title analytics.accounts
#' @description List accounts available to OAuth user
#' @param accessToken String OAuth token
#' @import data.table
#' @export
### 
analytics.accounts = function (accessToken) {
  query <- paste(
    "https://content.googleapis.com/analytics/v3/management/accountSummaries", 
    sep = ""
  );
  queryList = list(
    'access_token'= accessToken
  );

  req <- httr::GET(query, query = queryList);
  httr::stop_for_status(req);

  accts = parseAccounts(httr::content(req)$items);

  return(accts);
}

#' @title analytics.account.profiles
#' @description List profiles available to account
#' @param account String OAuth token
#' @param context Boolean provide extra context
#' 
#' @export
### 
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
  
  acctsTable = data.table::as.data.table(t(data.table::as.data.table(accts)));
  colnames(acctsTable) = names(accts[[1]]);
    
  for(acct in accts) {
    acctList = do.call(rbind, lapply(acct$webProperties, function(x) {
      p = rlist::list.stack(x$profiles);
      rownames(p) <- rlist::list.names(x$profiles[[1]][[1]]);
      x$profiles = data.table::as.data.table(t(data.table::as.data.table(t(p))));
      colnames(x$profiles) = colnames(p);
      return(x);
    }));
    
    acctList = cbind(name = acct$name, accountID = acct$id, acctList);
    acctsList = rbind( data.table::as.data.table(acctList), acctsList );
  }
  
  return(acctsList)
}