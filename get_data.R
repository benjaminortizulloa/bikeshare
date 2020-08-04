
df <- data.frame(
  month = rep(1:12, 3),
  year = as.vector(vapply(c(2018, 2019, 2020), function(x){rep(x, 12)}, double(12))),
  value = sample(c(1,2,3), 36, T)
)

write.csv(df, '../multi_year_test.csv', row.names = F)

library(aws.s3)

Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAI4IDYICHYJ6ZD5WA",
           "AWS_SECRET_ACCESS_KEY" = "fLNbuEcs4xenyosmqIDzXW8KA3AjT/Pnuy6nJUn3")

cbs <- get_bucket(bucket = 'capitalbikeshare-data')

yoi <- vapply(cbs, function(x){x$Key}, character(1)) %>%
  stringr::str_detect('^2019|2018|2017')

cbs_yoi <- cbs[yoi]

test <- get_object(FUN = function(x){writeBin(x, 'test.zip')}, object = "201912-capitalbikeshare-tripdata.zip", bucket = "capitalbikeshare-data")

test <- get_object(cbs_yoi[[1]])
