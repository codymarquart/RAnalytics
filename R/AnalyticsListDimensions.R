### Find all of the custom dimensions associated with the 
### account.
#listCustomDimensions
#accountID = "39236781", webID = "UA-39236781-3"
#ga$accounts()[[2]]$id, ga$accounts()[[2]]$webProperties[[4]]$id
#
analytics.list.dimensions = function (accessToken, account = NULL, accountID = NULL, webID = NULL, startIndex = 1, maxResults = 10) {
  if(!is.null(account)) {
    if(is.null(accountID)) {
      accountID = account$accountID;
    }
    if(is.null(webID)) {
      webID = account$id;
    }
  }

  query <- paste(
    "https://www.googleapis.com/analytics/v3/management/accounts/", accountID, "/webproperties/", webID, "/customDimensions", 
    sep = ""
  );

  queryList = list(
    'start-index'= startIndex,
    'max-results'= maxResults,
    'access_token'= accessToken #ga_token$credentials$access_token
  );
  print(query)
  req <- httr::GET(query, query = queryList);
  httr::stop_for_status(req);

  #return(content(req)$items);
  #return(rownames(list.table(content(req)$items, id)));

  dims = httr::content(req)$items;

  dimFrame = data.frame();
  for(item in dims) {
    dimFrame = rbind(dimFrame, data.frame(name=item$name, id=item$id))
  }
  rownames(dimFrame) <- dimFrame$id;

  return(dimFrame);
  #return(cbind(rownames(list.table(dims, name)),rownames(list.table(dims, id))));
}