### Save results as file
analytics.results.save = function(x, file = "./data/saved.csv", type = tools::file_ext(file)) {
  print(paste("Merged: ", is.null(x$merged)));
  
  toSave = NULL;
  if(is.null(x$merged)) {
    toSave = x;
  } else {
    toSave = x$merged;
  }
 
  if(type == "csv") {
    write.csv(toSave, file = file, fileEncoding = "UTF-8");
  } else {
    rlist::list.save(toSave, file, type);
  }
}