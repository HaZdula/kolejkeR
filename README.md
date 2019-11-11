# kolejkeR
The kolejkeR package is an R wrapper for the Otwarte dane po warszawsku (Open Warsaw Data) API. Project for Advanced R classes at the Warsaw University of Technology.

[![Travis build status](https://travis-ci.org/HaZdula/kolejkeR.svg?branch=master)](https://travis-ci.org/HaZdula/kolejkeR)
[![Coverage status](https://codecov.io/gh/HaZdula/kolejkeR/branch/master/graph/badge.svg)](https://codecov.io/gh/HaZdula/kolejkeR?branch=master)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("HaZdula/kolejkeR")
```

## Example


Let's assume, that you would like to file an application for 500+ programme in municipal office in Bielany in Warsaw. You wish to find out the average time you need to wait and the amount of people already waiting.

``` r
library(kolejkeR)
library(stringi)

offices <- get_available_offices()

# Find out the exact name of the office
Bielany_office <- offices[stri_detect_fixed(offices, "Bielany")][1]
queues <- get_available_queues(Bielany_office)

# Find the exact name of the queue
queue500 <- queues[stri_detect_fixed(queues, "500+")][1]

print(get_waiting_time(Bielany_office, queue500))
print(get_number_of_people(Bielany_office, queue500))

```
Now you can go to the municipal office with a big smile on your face.

