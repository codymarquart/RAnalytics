gaToken = analytics.connect("./tools/testing.json")
analytics.by.day = function(start = "2015/12/1", end = format(Sys.time(), "%Y/%m/%d"), ...) {
  print(seq(as.Date(start), as.Date(end), "days"))
  
  result = analytics.query(...)
  
  View(result$merged)
}

result = analytics.query(accessToken = gaToken, dimensions = dims$id, startDate = "2016-09-12", endDate = "2016-09-12", uniqueBy = uniqueBy, columnNames = dims, accountID = "39236781", webID = "UA-39236781-3")
