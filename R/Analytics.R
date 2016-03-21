###
#' @title Main function for connection to Google Analytics
#' @description Wrapper for Analytics functions
#' @param json String file location to load JSON data from
#' @param jsonText String json representation
#' @param set_global Boolean
#' @param set_global_var String to use for name of the global
#' 
#' @include AnalyticsConnect.R
#' @include AnalyticsListDimensions.R
#' @export
GA = function(
  json = NULL, 
  jsonText = NULL, 
  set_global = FALSE, 
  set_global_var = "GA"
) {
  GA_object(json, jsonText, set_global, set_global_var);
}

GA_object = function(
  json = NULL, 
  jsonText = NULL, 
  set_global = FALSE, 
  set_global_var = "GA"
) {
  self <- local({

    connection <- analytics.connect(json, jsonText);

    reconnect <- function(
      json = NULL,
      jsonText = NULL,
      useCache = FALSE
    ) {
      if(useCache == TRUE && !is.null(connection)) {
        return(connection);
      } else {
        connection <- analytics.connect(json, jsonText);
      }
    };
    
    getToken <- function(data, pagesize = 1000) {
      return(connection);
    };

    dimensions <- NULL;
    
    getDimensions <- function(account = NULL, accountID = NULL, webID = NULL) {
      dimensions = analytics.list.dimensions(
        accessToken = connection, 
        account = account, 
        accountID = accountID, 
        webID = webID
      );

      return(dimensions);
    };

    save.results <- function(
      x, 
      file = "./data/saved.csv", 
      type = tools::file_ext(file)
    ) {
      analytics.results.save(x, file, type);
    };
    
    query <- function(
      accessToken = connection,
      account = NULL,
      accountID = NULL, 
      webID = NULL,
      ids = NULL,
      startDate = "30daysAgo",
      endDate = "today",
      dimensions = NULL, #c("ga:eventLabel"),
      metrics = c("ga:eventValue"),
      columnNames = dimensions[,1],
      uniqueBy = NULL,
      startIndex = 1,
      maxResults = 20000,
      appendEvents = TRUE
    ) {
      return(
        analytics.query(
          accessToken,ids,startDate,endDate,dimensions = dimensions,
          metrics,columnNames,uniqueBy,startIndex,maxResults, appendEvents
        )
      );
    };

    accounts <- analytics.accounts(connection);

    #getAccounts <- function(accessToken = connection) {
    #  return(analytics.accounts(accessToken));
    #};
    
    accountProfiles <- function(account) {
      return(analytics.account.profiles(account));
    };

    environment();
  });
  
  lockEnvironment(self, TRUE)
  structure(self, class=c("GA", "egr", class(self)))
}
