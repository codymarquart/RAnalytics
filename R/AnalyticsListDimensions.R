#' @title Analytics account dimenstions
#' @description Find all of the custom dimensions associated with the 
#' account.
#'
#' @param accessToken OAUTH Token
#' @param account Full account to pull ID and webID from
#' @param accountID account ID string
#' @param webID web ID string
#' @param startIndex Integer
#' @param maxResults Integer
#' 
#' @export
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

  req <- httr::GET(query, query = queryList);
  httr::stop_for_status(req);

  dims = httr::content(req)$items;

  dimFrame = data.frame();
  for(item in dims) {
    dimFrame = rbind(dimFrame, data.frame(name=item$name, id=item$id))
  }
  rownames(dimFrame) <- dimFrame$id;

  return(dimFrame);
  #return(cbind(rownames(list.table(dims, name)),rownames(list.table(dims, id))));
}