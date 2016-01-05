###
# libaries: rjson, rlist, httr
#
#'
#'
#' @export
GA = function(json = NULL, jsonText = NULL, set_global = FALSE, set_global_var = "GA") {
  GA_object(json, jsonText, set_global, set_global_var);
}

GA_object = function(json = NULL, jsonText = NULL, set_global = FALSE, set_global_var = "GA") {
  self <- local({

    connection <- analytics.connect(json, jsonText, set_global, set_global_var);

    reconnect <- function(json = NULL, jsonText = NULL, set_global = FALSE, set_global_var = "GA", useCache = FALSE) {
      if(useCache == TRUE && !is.null(connection)) {
        return(connection);
      } else {
        connection <- analytics.connect(json, jsonText, set_global, set_global_var);
      }
    };
    
    getToken <- function(data, pagesize = 1000) {
      return(connection);
    };

    dimensions <- NULL;
    
    getDimensions <- function(account = NULL, accountID = NULL, webID = NULL) {
      dimensions = analytics.list.dimensions(connection, account = account, accountID = accountID, webID = webID);
      return(dimensions);
    };

    save.results <- function(x, file = "./data/saved.csv", type = tools::file_ext(file)) {
      analytics.results.save(x, file, type);
    };
    
    query <- function(
      accessToken = connection,
      account = NULL,
      accountID = NULL, 
      webID = NULL,
      ids = "113385228",
      startDate = "30daysAgo",
      endDate = "today",
      dims = NULL, #c("ga:eventLabel"),
      metrics = c("ga:eventValue"),
      columnNames = dimensions[,1],
      uniqueBy = NULL,
      startIndex = 1,
      maxResults = 1000,
      appendEvents = TRUE
    ) {
      if(is.null(dims)) {
        dims = getDimensions(account, accountID, webID);
        return(analytics.query(accessToken,ids,startDate,endDate,dims = levels(dims$id),metrics,columnNames,uniqueBy,startIndex,maxResults, appendEvents));
      } else {
        return(analytics.query(accessToken,ids,startDate,endDate,dims = dims,metrics,columnNames,uniqueBy,startIndex,maxResults, appendEvents));
      }

    };

    accounts <- analytics.accounts(connection);

    getAccounts <- function(accessToken = connection) {
      return(analytics.accounts(accessToken));
    };
    
    accountProfiles <- function(account) {
      return(analytics.account.profiles(account));
    };

    environment();
  });
  
  lockEnvironment(self, TRUE)
  structure(self, class=c("GA", "egr", class(self)))
}
