% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_rtgtfs_trip_vehicle_ids.R
\name{check_rtgtfs_trip_vehicle_ids}
\alias{check_rtgtfs_trip_vehicle_ids}
\title{Check if the vehicle positioning data has the correct amount of trip and vehicle IDs}
\usage{
check_rtgtfs_trip_vehicle_ids(data)
}
\arguments{
\item{data}{A data frame containing the vehicle positioning data.}
}
\value{
The maximum count of trip IDs found in the data.
}
\description{
This function takes a data frame as input and checks if the vehicle positioning data
contains the correct number of trip and vehicle IDs. It provides a message to the user
indicating the outcome of the check and notifies that the function has started.
}
\examples{
data <- read.csv("vehicle_data.csv")
check_rtgtfs_trip_vehicle_ids(data)

}
