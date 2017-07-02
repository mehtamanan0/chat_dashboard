message_query_generator <- function(channel, start_time, end_time){
query <- sprintf('{"query": {"bool": 
                {"must": [
                { "match": { "data.business_via_name": "%s" }},
                {"range": {"data.created_at": { "gte": "%s", "lte": "%s"}}},
                { "match": { "type": "message" }},
                {"range": {"data.total_user_messages": {"gt": 0}}
                }],
           "must_not": { "match": { "data.message_by": "Others" }}}
          }
  }',channel, start_time, end_time, channel)
return(query)
}
stats_query_generator <- function(channel, start_time, end_time){
  query <- sprintf('{"query": {
    "bool": {
      "must": [
        { "match": { "data.business_via_name": "%s" }},
        { "range": {"data.created_at": {"gte": "%s","lte": "%s"}}},
        { "match": { "type": "chat" }}
        ]
      }
    }}',channel, start_time, end_time)
  return(query)
}

elastic_get_data <- function(index, type, query, size){
  res <-Search(index = index, type=type, scroll="5m", body=query, asdf=TRUE, size=size)
  response <-res$hits$hits$`_source`$`data`
  print(paste('number of hits ',length(response)))
  hits <- 1
  while(hits != 0){
    #res1 <- scroll(scroll_id = res$`_scroll_id`, asdf=TRUE)$hits$hits$`_source` # this does not work
    res1 <- scroll(scroll_id = res$`_scroll_id`, asdf=TRUE)
    response1 <- res1$hits$hits$`_source`$`data`
    hits <- length(response1)
    if(hits > 0){
      response <- c(response, response1)
      print(hits)
    }
  } 
  return(response)
}










