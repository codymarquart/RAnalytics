### Connect to Google API OAuth
analytics.connect = function(json = NULL, jsonText = NULL, set_global = FALSE, set_global_var = "GA") {
  if(is.null(json) && is.null(jsonText)) {
    print("Must provide a JSON credentials file: https://console.developers.google.com/apis/credentials")
    return();
  }
  
  jsonSettings = NULL;
  if(!is.null(json)) {
    if(file.exists(json)) {
      jsonSettings = fromJSON(file = json);
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
  ga_tokens <- httr::oauth2.0_token(ga, myapp, scope=c("https://www.googleapis.com/auth/analytics", "https://www.googleapis.com/auth/analytics.edit"), cache = FALSE);
  
  if(set_global == TRUE) {
    GA = NULL;
    if(!exists(set_global_var, envir = .GlobalEnv)) {
      GA = list();
    }

    GA$tokens = ga_tokens;
    assign(set_global_var, GA, envir = .GlobalEnv);
  }

  return(getToken(ga_tokens, "access_token"));
}

### Retrieve token
getToken = function(tokens = NULL, token = "access_token", global_var = "GA") {
  if(is.null(tokens)) {
    GA = get("GA", envir = .GlobalEnv);
    tokens = GA$tokens
  }
  token = tokens$credentials$access_token;

  return(token);
}