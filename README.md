# Caltrans claims data

The data and analysis in this repository fueled the [CBS News California investigation into Caltrans claims](https://www.cbsnews.com/sacramento/news/caltrans-pothole-damage-claims-california-freeways/). The data was provided by Caltrans via a California Public Records Act request, reference number R024170-062723, submitted on June 27, 2023. 

The folders and files included in this repo are as follows:

*[caltrans-district-data](https://github.com/cbs-news-data/caltrans-claims-data/tree/main/caltrans-district-data): .csvs with information about caltrans districts
*[geo-data](https://github.com/cbs-news-data/caltrans-claims-data/tree/main/geo-data): Geographic data for county boundaries and state highways. ([Source for highway data](https://gis.data.ca.gov/datasets/77f2d7ba94e040a78bfbe36feb6279da_0/explore?location=36.988057%2C-119.281578%2C6.08))
*[output](https://github.com/cbs-news-data/caltrans-claims-data/tree/main/output): Output files from analysis. 
⋅⋅*Includes a (relatively) [clean version of the raw data from Caltrans](https://github.com/cbs-news-data/caltrans-claims-data/blob/main/output/caltrans_data_clean.csv). Cleaned/added columns include the incident date and year, a cleaned version of the "status," "route," and "amount" columns. Other added columns include whether or not the claim mention potholes or construction work, and if the amount paid/sought/status columns made sense in relation to each other.
*[scripts](https://github.com/cbs-news-data/caltrans-claims-data/tree/main/scripts): All the R scripts used for analysis. They are numbered in the order they should be run in. 