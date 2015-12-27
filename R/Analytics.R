###
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
    
    getDimensions <- function(accountID, webID) {
      dimensions = analytics.list.dimensions(connection, accountID, webID);
      return(dimensions);
    };

    save.results <- function(x, file = "./data/saved.csv", type = tools::file_ext(file)) {
      analytics.results.save(x, file, type);
    };
    
    query <- function(
      accessToken = connection,
      accountID, webID,
      ids = "113385228",
      startDate = "30daysAgo",
      endDate = "today",
      dims = NULL, #c("ga:eventLabel"),
      metrics = c("ga:eventValue"),
      columnNames = dimensions[,1],
      uniqueBy = NULL,
      startIndex = 1,
      maxResults = 1000
    ) {
      if(is.null(dimensions)) {
        dims = getDimensions(accountID, webID);
      }

      return(analytics.query(accessToken,ids,startDate,endDate,dims[,2],metrics,columnNames,uniqueBy,startIndex,maxResults));
    };

    accounts <- analytics.accounts(connection);

    getAccounts <- function(accessToken = connection) {
      return(analytics.accounts(accessToken));
    }
    
    environment();
  });
  
  lockEnvironment(self, TRUE)
  structure(self, class=c("GA", "egr", class(self)))
}

