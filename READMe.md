# Load required R packages (must be installed first)
```r
install.packages(c("magrittr", "jsonlite", "httr"))
require(magrittr)
require(jsonlite)
require(httr)
```


# Main Functions

## Parse Server Response
```r
parseResponse <- function(server_resp) {return(fromJSON(content(server_resp, "text", encoding="UTF-8")))}
```

## Function to Check credits usage
```r
check_credits <- function(api_key) {
  validate_endpoint = 'https://validator.extracttable.com'
  return(content(GET(url = validate_endpoint, add_headers(`x-api-key` = api_key)), as = 'parsed', type = 'application/json'))
}
```

## Function to Retrieve the result by JobId
```r
retrieve_result <- function(api_key, job_id) {
  retrieve_endpoint = "https://getresult.extracttable.com"
  return(
    GET(
      url = paste0(retrieve_endpoint, "/?JobId=", job_id),
      add_headers(`x-api-key` = api_key)
      )
    )
}
```

## Function to trigger a file for extraction
```r
proces_file <- function(api_key, filepath) {
  trigger_endpoint = "https://trigger.extracttable.com"
  return (
    POST(
      url = trigger_endpoint,
      add_headers(`Content-Type`="multipart/form-data", `x-api-key` = api_key),
      body = list(input = upload_file(filepath))
    )
  )
}
```

## Function to extract tables from the input file
```r
ExtractTable <- function(filepath, api_key) {
  server_response <- proces_file(api_key, filepath)
  parsed_resp = parseResponse(server_response)


  # Wait for a maximum of 5 minutes to finish the trigger job
  # Retries every 20 seconds
  max_wait_time = 5*60
  retry_interval = 20
  while (parsed_resp$JobStatus == 'Processing' & max_wait_time >= 0) {
    max_wait_time = max_wait_time - retry_interval
    print(paste0("Job is still in progress. Let's wait for ", retry_interval, " seconds"))
    Sys.sleep(retry_interval)
    server_response <- retrieve_result(api_key, job_id=parsed_resp$JobId)
    parsed_resp = parseResponse(server_response)
  }

  ### Parse the response for tables
  et_tables <- content(server_response, as = 'parsed', type = 'application/json')

  all_tables <- list()

  if (tolower(parsed_resp$JobStatus) != "success") {
    print(paste0("The processing was NOT SUCCESSFUL Below is the complete response from the server"))
    print(parsed_resp)
    return(all_tables)
  }

  ### Convert the extracted tabular JSON data as a dataframe for future use
  ### Each data frame represents one table
  for (i in 1:length(et_tables$Table)) {
    all_tables[[i]] <- sapply(et_tables$Tables[[i]]$TableJson, unlist) %>% t() %>% as.data.frame()
  }

  return(all_tables)
}
```


# Usage
```r
## Intialize valid API key received from https://extracttable.com
api_key = YOUR_VALID_API_KEY_HERE

# Validate or check credits of the API key
credits <- check_credits(api_key = api_key)$usage
credits

# Input location
input_location = LOCATION_OF_YOUR_INPUT_IMAGE_HERE

# Trigger the job for processing and get results as an array of dataframes
# Each data frame represents one table
results <- ExtractTable(api_key = api_key, filepath = input_location)
```

*Thanks to one of our beloved user, Roeland van der Molen (lmconsultants.nl), supported us with the initial R code which motivated us to put efforts in this write up*
