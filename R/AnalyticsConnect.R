### Connect to Google API OAuth
#
#' @param set_global Boolean, when true, sets the resulting 
#'' @export
#' @importFrom httr oauth_endpoint
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' @importFrom json fromJSON
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
    fromJSON(jsonText)
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
  
  ga <- oauth_endpoint( authorize = endpointAuthorize, access = endpointAccess);
  myapp <- oauth_app("ga",
    key = clientID, # Client ID
    secret = clientSecret # Client Secret
  );
  ga_tokens <- oauth2.0_token(ga, myapp, scope=c("https://www.googleapis.com/auth/analytics", "https://www.googleapis.com/auth/analytics.edit"), cache = FALSE);
  
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
#'
#' \code{getToken} Returns the token for the provided
#' tokens object: 
#'
#'' @export
getToken = function(tokens = NULL, token = "access_token", global_var = "GA") {
  if(is.null(tokens)) {
    GA = get("GA", envir = .GlobalEnv);
    tokens = GA$tokens
  }
  token = tokens$credentials$access_token;

  return(token);
}