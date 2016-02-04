### Query the Analytics API
analytics.query = function(
  accessToken = NULL,
  ids = NULL,
  startDate = "30daysAgo",
  endDate = "today",
  dims = c("ga:eventAction"),
  metrics = c("ga:eventValue"),
  columnNames = NULL,
  uniqueBy = NULL,
  startIndex = 1,
  maxResults = 1000,
  appendEvents = TRUE
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
  
  if(appendEvents == TRUE) {
    dims = append(dims, c("ga:eventAction", "ga:eventLabel", "ga:eventCategory"));
  }

  # TODO Maybe allow for the user to select the columns at this point
  if(is.null(uniqueBy) && length(dims) > 7) {
    print("WARNING: In order to use more than 7 dimensions, use uniqueBy so multiple queries can be merged together.");
    return();
  } else if(!is.null(uniqueBy) && length(dims) > 7) { }

  start = 1;
  results = list();

  queryURL = "https://www.googleapis.com/analytics/v3/data/ga";
  repeat {
    result = list();
    result$dimensionsUsed = rlist::list.cases(append(uniqueBy, Filter(function(x) { return(x != "NA"); }, dims[start:(start+(6 - length(uniqueBy)))])));
    start = start + (7 - length(uniqueBy));
    print(result$dimensionsUsed)
    queryList = list(
      'ids'= ids,
      'start-date' = startDate,
      'end-date'= endDate,
      'dimensions'= paste(result$dimensionsUsed, collapse=","),
      'metrics'= paste(metrics, collapse=","),
      'start-index'= startIndex,
      'max-results'= maxResults,
      'access_token'= accessToken
    );
    #print(queryURL);
    #print(queryList);

    result$req <- httr::GET(queryURL, query = queryList);
    httr::stop_for_status(result$req);

    df = NULL;
    if(!is.null(httr::content(result$req)$rows)) {
      df <- do.call(rbind.data.frame, httr::content(result$req)$rows);
      colnames(df) = sapply(httr::content(result$req)$columnHeaders, function(x) x[1]);
    }
    result$data <- df;

    results = rlist::list.append(results, result);
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
    
    if(length(newColnames) > 0) {
      colnames(results$merged) <- newColnames;
    }
  }
  
  return(results);
}