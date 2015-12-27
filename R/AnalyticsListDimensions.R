### Find all of the custom dimensions associated with the 
### account.
#'
#'' @importFrom rlist list.table
#'' @export
#
#listCustomDimensions
#accountID = "39236781", webID = "UA-39236781-3"
#ga$accounts()[[2]]$id, ga$accounts()[[2]]$webProperties[[4]]$id
#
analytics.list.dimensions = function (accessToken, accountID = NULL, webID = NULL, startIndex = 1, maxResults = 10) {
  query <- paste(
    "https://www.googleapis.com/analytics/v3/management/accounts/", accountID, "/webproperties/", webID, "/customDimensions", 
    sep = ""
  );

  queryList = list(
    'start-index'= startIndex,
    'max-results'= maxResults,
    'access_token'= accessToken #ga_token$credentials$access_token
  );

  req <- GET(query, query = queryList);
  stop_for_status(req);

  #return(content(req)$items);
  #return(rownames(list.table(content(req)$items, id)));
  dims = content(req)$items;

  return(cbind(rownames(list.table(dims, name)),rownames(list.table(dims, id))));
}