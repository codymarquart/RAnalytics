#' @title analytics.connect
#' @description Connect to Google API OAuth
#' @param json File location
#' @param jsonText String representation of JSON
#' @import rjson httr
#' @export
### 
analytics.connect = function(
  json = NULL,
  jsonText = NULL
) {
  if(is.null(json) && is.null(jsonText)) {
    print("Must provide a JSON credentials file: https://console.developers.google.com/apis/credentials")
    return();
  }
  
  jsonSettings = NULL;
  if(!is.null(json)) {
    if(file.exists(json)) {
      jsonSettings = rjson::fromJSON(file = json);
    } else {
      print("Provided file doesn't exist");
      return();
    }
  } else if (!is.null(jsonText)) {
    rjson::fromJSON(jsonText)
  } else {
    print("Unable to parse JSON from either `json` or `jsonText`")
    return()
  }
  
  clientID = jsonSettings$installed$client_id;
  clientSecret = jsonSettings$installed$client_secret;
  endpointAuthorize = jsonSettings$installed$auth_uri;
  endpointAccess = jsonSettings$installed$token_uri;
  
  if(is.null(clientID) || is.null(clientSecret) || is.null(endpointAuthorize) || is.null(endpointAccess) ) {
    print("Settings file may be corrupt, not all items present.")
    return();
  }
  
  ga <- httr::oauth_endpoint( authorize = endpointAuthorize, access = endpointAccess);
  myapp <- httr::oauth_app("ga",
    key = clientID, # Client ID
    secret = clientSecret # Client Secret
  );
  ga_tokens <- httr::oauth2.0_token(ga, myapp, scope=c(
      "https://www.googleapis.com/auth/analytics",
      "https://www.googleapis.com/auth/analytics.edit"
    ), 
    cache = FALSE #"./tools/httr-oauth"
  );

  return(getToken(ga_tokens, "access_token"));
}

getToken = function(tokens = NULL, token = "access_token", global_var = "GA") {
  if(is.null(tokens)) {
    GA = get("GA", envir = .GlobalEnv);
    tokens = GA$tokens
  }
  token = tokens$credentials$access_token;

  return(token);
}