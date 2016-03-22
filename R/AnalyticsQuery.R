#' @title analytics.query
#' @description Query the Analytics API showing the first 10,000 results from
#' the last 30 days.
#'
#' @param accessToken OAUTH Token
#' @param ids String
#' @param startDate starting date for query results
#' @param endDate end date for query results
#' @param dimensions List of dimensions to query
#' @param metrics List of metrics to query
#' @param columnNames List of names to put on the results
#' @param uniqueBy List holding columns that indicate uniqueness
#' @param startIndex Integer index of which to start
#' @param maxResults Integer for total number of results
#' @param appendEvents Boolean
#' @param accountID String
#' @param webID String
#' @export
analytics.query = function(
  accessToken = NULL,
  ids = NULL,
  startDate = "30daysAgo",
  endDate = "today",
  dimensions = c("ga:eventAction"),
  metrics = c("ga:eventValue"),
  columnNames = NULL,
  uniqueBy = NULL,
  startIndex = 1,
  maxResults = 10000,
  appendEvents = TRUE,
  accountID = NULL,
  webID = NULL
) {
  
  if(is.null(accessToken)){
    accessToken = getToken();

    if(is.null(accessToken)) {
      print("FAILED: No Token.");
      return();
    }
  }
  
  if(is.null(ids)) {
    acctsAll = analytics.accounts(accessToken = accessToken);
    acctTable = as.data.frame(unlist(unique(acctsAll$name)))
    colnames(acctTable) = c("Account")
    print(acctTable)
    acctChoice = readline(prompt = "Which account: ");
    
    acctNames = acctsAll[acctsAll$name == data.table::as.data.table(unlist(unique(acctsAll$name)))$V1[as.integer(acctChoice)],]
    print(acctNames);
    
    propertyChoice = readline(prompt = "Which property: ");
    accountID = acctNames[as.integer(propertyChoice),]$accountID;
    webID = acctNames[as.integer(propertyChoice),]$id;
    
    print(acctNames[as.integer(propertyChoice),]$profiles[[1]])
    viewChoice = as.integer(readline(prompt = "Which view: "));

    ids = acctNames[as.integer(propertyChoice),]$profiles[[1]]$id[viewChoice]

    print(paste("ID selected: ", ids))
  }

  if(is.null(ids)) {
    print("FAILED: No ID provided. Use https://ga-dev-tools.appspot.com/account-explorer/");
    return();
  }
  ids = paste("ga:", ids, sep = "");
  
  dimensionsAll = NULL
  if(is.logical(dimensions) && dimensions == TRUE) {
    dimensionsAll = analytics.list.dimensions(accessToken = accessToken, accountID = accountID, webID = webID)
    dimensions = dimensionsAll$id
  }

  if(appendEvents == TRUE) {
    dimensions = append(levels(factor(dimensions)), c("ga:eventAction", "ga:eventLabel", "ga:eventCategory"));
  }
  if(is.null(columnNames)) {
    #assign("D", dimensionsAll$name, envir = .GlobalEnv)
    columnNames = dimensionsAll
    print(columnNames)
  }

  start = 1;
  results = list();

  queryURL = "https://www.googleapis.com/analytics/v3/data/ga";
  repeat {
    result = list();
    result$dimensionsUsed = rlist::list.cases(append(uniqueBy, levels(factor(dimensions[start:(start+(6 - length(uniqueBy)))]))))
    start = start + (7 - length(uniqueBy));
  
    print("Querying dimensions:")
    print(result$dimensionsUsed);
    print("")
    queryList = list(
      'ids'= ids,
      'start-date' = startDate,
      'end-date'= endDate,
      'dimensions'= paste(result$dimensionsUsed, collapse=","),
      'metrics'= paste(metrics, collapse=","),
      'start-index'= startIndex,
      'include-empty-rows' = TRUE,
      'max-results'= maxResults,
      'access_token'= accessToken
    );

    result$req <- httr::GET(queryURL, query = queryList);
    httr::stop_for_status(result$req);

    df = NULL;
    if(!is.null(httr::content(result$req)$rows)) {
      df <- do.call(rbind.data.frame, httr::content(result$req)$rows);
      colnames(df) = sapply(httr::content(result$req)$columnHeaders, function(x) x[1]);
    }
    result$data <- df;

    results = rlist::list.append(results, result);
    if(start > length(dimensions)) {
      break;
    }
  }

  if(is.null(uniqueBy) && length(dimensions) > 7) {
    print("WARNING: In order to use more than 7 dimensions, use uniqueBy so multiple queries can be merged together.");
  } else if(!is.null(uniqueBy) && length(dimensions) > 7) { 
    print(paste("Merging the results by: ", uniqueBy));
    mergedResults = NULL;
    for (r in results) {
      if(is.null(mergedResults)) { 
        print("Setting initial results.");
        mergedResults = r$data; 
      }
      else {
        mergedResults = merge(mergedResults, r$data, by = uniqueBy, all.y = TRUE, all.x = TRUE)
      }
    }
    results$merged = mergedResults;
  
    if(!is.null(columnNames)) {
      newColnames = list();
      newColnameIndex = 1;
      
      for(col in colnames(mergedResults)) {
        colName = levels(factor(columnNames[c(col),]$name));
  
        if(length(colName) > 0) {
          newColnames[[newColnameIndex]] = colName;
        } else {
          newColnames[[newColnameIndex]] = col;
        }
        newColnameIndex = newColnameIndex + 1;
      }
      
      if(length(newColnames) > 0) {
        colnames(results$merged) <- newColnames;
      }
    }
  }

  return(results);
}