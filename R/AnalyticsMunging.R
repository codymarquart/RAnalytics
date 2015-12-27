#library(devtools);
#library(httr);

### Process the result of data pulls
#
#'' @export
process = function() {
  LiveUsers = read.csv(file = '~/Downloads/live-users.csv')
  colnames(LiveUsers) <- c("UserID", "username");
  
  MetaTab = read.csv(file = '~/Downloads/MetaTab-20151214-20151215.csv');
  MetaTab2 = read.csv(file = '~/Downloads/MetaTab2-20151214-20151215.csv');
  EventTab = read.csv(file = '~/Downloads/EventTab-20151214-20151215.csv');
  
  #MetaTab = read.csv(file = '~/Downloads/MetaTab-20151215.csv');
  #MetaTab2 = read.csv(file = '~/Downloads/Analytics All Web Site Data Full Report 20151206-20151216 (1).csv');
  #EventTab = read.csv(file = '~/Downloads/Analytics All Web Site Data Full Report 20151206-20151216 (2).csv');
  
  MetaTabUsernames <- merge(LiveUsers, MetaTab, by="UserID")
  MetaTabUsernames$Hit.Time = ms.to.date(MetaTabUsernames$Hit.Time/1000);
  
  AllMeta = merge(MetaTabUsernames, MetaTab2, by="UniqueHitID");
  AllMetaAndEvents = merge(AllMeta, EventTab, by="UniqueHitID");
}

getEvents = function() {
  return(c("ga:dimension2","ga:dimension8","ga:eventAction","ga:eventCategory","ga:eventLabel"));
}
getMetaTab = function() {
  return(c("ga:dimension2","ga:dimension8","ga:dimension3","ga:dimension4","ga:dimension5","ga:dimension6","ga:dimension7"));
}

### Convert the query results into a data frame
#
#'' @export
viewResults = function(x, req = "req") {
  if(is.null(x)) {
    x = get(req, envir = .GlobalEnv);
  }
  
  df = NULL;
  if(!is.null(content(x)$rows)) {
    df <- do.call(rbind.data.frame, content(x)$rows);
    colnames(df) = sapply(content(x)$columnHeaders, function(x) x[1]);
  }
  
  return(df);
}