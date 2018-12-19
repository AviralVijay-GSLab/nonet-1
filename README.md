# nonet

nonet is unified solution for weighted average ensemble in supervised and unsupervised learning environment. It is a novel approach to provide weighted average ensembled predictions without using labels from outcome or response variable for weight computation. In a nutshell, nonet can be used in two scenarios: 

- This approach can be used in the unsupervised environment where outcome labels not available.
- This  approach can be used to impute the missing values in the real-time scenarios in supervised and unsupervised environment because nonet does not require training labels to compute the weights for ensemble. 

## How to run locally:
This tool uses the [shiny framework](https://shiny.rstudio.com/) for visualizing events.
In order to run it, you need to have [R](https://mran.microsoft.com/download) and preferably [Rstudio](https://www.rstudio.com/products/rstudio/download/).
Once you have everything installed, open the project on R studio and click `Run App`, or call runApp() from the console. You might need to manually install the required packages

#### Requirements
- R (<= 3.5.1 && above 3.4.0)

#### Used packages: 
- caret (>= 6.0.78),
- dplyr,
- randomForest,
- ggplot2,
- rlist (>= 0.4.6.1),
- glmnet,
- tidyverse,
- ClusterR,
- e1071,
- purrr,
- pROC (>= 1.13.0),
- rlang (<= 0.3.0.1),

## How to deploy using docker:
Option 1: [Deploy to Azure Web App for Containers or Azure Container Instances](https://azuredeploy.net/). More details [here (webapp)](https://azure.microsoft.com/en-us/services/app-service/containers/) and [here (container instances)](https://azure.microsoft.com/en-us/services/container-instances/)

Option 2: Deploy [this image](https://hub.docker.com/r/omri374/taganomaly/) to your own environment.

### Dockerize the shiny app:
Follow the steps on [rize](https://github.com/cole-brokamp/rize) on how to deploy on shiny-server. Default port is `3838`, so make sure you have it open or change the default port to something else.

## Instructions of use
1. Import time series CSV file. Assumed structure:
- date (`"%Y-%m-%d %H:%M:%S"`). TagAnomaly will attempt to infer the date from other patterns as well, using the *parsedate* package
- category (optional)
- value

2. (Optional) Import raw data time series CSV file.

If the original time series is an aggreation over time windows, this time series is the raw values themselves. This way we could dive deeper into an anomalous value and see what it is comprised of.
Assumed structure:
- date (`"%Y-%m-%d %H:%M:%S"`). TagAnomaly will attempt to infer the date from other patterns as well, using the *parsedate* package
- category (optional)
- content

2. Select category (optional, if exists)

3. Select time range on slider

4. Select points on plot that look anomalous.
Optional (1): click on one time range on the table below the plot to see raw data on this time range
Optional (2): Open the `All Categories` tab to see how other time series behave on the same time range.
5. Once you decide that these are actual anomalies, save the resulting table to csv by clicking on `Download labels set` and continue to the next category.

#### Current limitations/issues
It is currently impossible to have multiple selections on one plot. A workaround is to select one area, download the csv and select the next area. Each downloaded CSV has a random string so files won't override each other. Once labeling is finished, one option is to run the provided [prep_labels.py](https://github.com/Microsoft/TagAnomaly/blob/master/prep_labels.py) file in order to concatenate all of TagAnomaly's output file to one CSV.
# Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.microsoft.com.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
