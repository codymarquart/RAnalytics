### Query the Analytics API
#
#' @param accessToken 
#' @param ids
#' @param startDate
#' @param endDate
#' @param dims
#' @param metrics
#' @param startIndex
#' @param maxResults
#'' @export
analytics.query = function(
  accessToken = NULL,
  ids = "113385228",
  startDate = "30daysAgo",
  endDate = "today",
  dims = c("ga:eventLabel"),
  metrics = c("ga:eventValue"),
  columnNames = NULL,
  uniqueBy = NULL,
  startIndex = 1,
  maxResults = 1000
) {
  if(is.null(accessToken)){
    accessToken = getToken();
    
    if(is.null(accessToken)) {
      print("FAILED: No Token.");
      return();
    }
  }
  if(is.null(ids)) {
    print("FAILED: No ID provided. Use https://ga-dev-tools.appspot.com/account-explorer/");
    return();
  }
  ids = paste("ga:", ids, sep = "");
  
  dims = append(dims, c("ga:eventAction", "ga:eventLabel"))
  
  if(is.null(uniqueBy) && length(dims) > 7) {
    print("In order to use more than 7 dimensions, use uniqueBy so multiple queries can be merged together.");
    return();
  } else if(!is.null(uniqueBy) && length(dims) > 7) {
    
  }

  start = 1;
  results = list();

  queryURL = "https://www.googleapis.com/analytics/v3/data/ga";
  repeat {
    result = list();
    result$dimensionsUsed = list.cases(append(uniqueBy, Filter(function(x) { return(x != "NA"); }, dims[start:(start+(6 - length(uniqueBy)))])));
    start = start + (7 - length(uniqueBy));
    queryList = list(
      'ids'= ids,
      'start-date' = startDate,
      'end-date'= endDate,
      'dimensions'= paste(result$dimensionsUsed, collapse=","),
      'metrics'= paste(metrics, collapse=","), # was a list -> metrics = [ ga:eventValue ]
      'start-index'= startIndex,
      'max-results'= maxResults,
      'access_token'= accessToken #ga_token$credentials$access_token
    );

    result$req <- GET(queryURL, query = queryList);
    stop_for_status(result$req);

    df = NULL;
    if(!is.null(content(result$req)$rows)) {
      df <- do.call(rbind.data.frame, content(result$req)$rows);
      colnames(df) = sapply(content(result$req)$columnHeaders, function(x) x[1]);
    }
    result$data <- df;

    results = list.append(results, result);
    if(start > length(dims)) {
      break;
    }
  }

  mergedResults = NULL;
  for (r in results) {
    if(is.null(mergedResults)) { mergedResults = r$data; }
    else { mergedResults = merge(mergedResults, r$data, by = uniqueBy); }
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

    colnames(results$merged) <- newColnames;
  }
  
  return(results);
}