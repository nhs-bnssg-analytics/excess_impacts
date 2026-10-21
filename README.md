# Excess Impacts of Delays Emergency Medical Admissions

This repository hosts the data generation script and shiny app for the Excess Impacts tool.

## Installation

Clone the repository and navigate into the project directory:

```bash
git clone git@github.com:nhs-bnssg-analytics/excess_impacts.git
```

*(Note: There is no project environment for this project at this stage. A list of required packages can be found in the `code_data.R` script, and this code will run using R version 4.5.1).*

## Usage

Generation of new impacts data is handled by `code_data.R` and the shiny app can be found at `shinyApp/app.R`. To note, executing this code requires system specific links to databases which are not provided within this repository.

Furthermore, these processes are hosted using docker, governed by various docker configuration files:

* `Dockerfile_icb`
* `docker-compose_icb.yml`
* `.data.Dockerfile`
* `.data.docker-compose.yml`



---

## Contact
**Author:** Nick Howlett, nick.howlett5@nhs.net