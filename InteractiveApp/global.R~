library(dplyr)

allbus = read.table("data/fullbusinessdetails.txt")
row.names(allbus) = allbus$business

cleantable <- allbus %>%
  select(
    Name = fullname,
    Hash = business,
    OverallReviewScore = totalreviewscore,
    OverallSampleSize = totalsamples,
    ServiceReviewScore = scorewsw,
    ServiceReviewSampleSize = sampwsw,
    NonServiceReviewScore = scorewosw,
    NonServiceReviewSampleSize = sampwosw,
    Pvalue = pvalues,
    Qvalue = qvalues,
    Latitude = lat,
    Longitude = long
  )
