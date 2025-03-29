# burn-severity-co
Predicting burn severity in Colorado watersheds

$${\color{red}**Please \space note: \space This \space repo \space is \space under \space development**}$$

# Overview

The series of scripts found here present a workflow for predicting wildfire severity with decision tree models in a pre-burn context across the state of Colorado.  We first train random forest models = on observed differenced Normalized Burn Ratio (dNBR) in xx fires across Colorado for the past xx years to developed a model capable of replicating observed dNBR from various ecologic, topographic, and climatic attributes. Then, we use those trained models to predict burn severity for currently unburned areas. This approach, in essence, allows for anticipation of burn severity should any given area burn sometime in the future, providing insights into potential future hazards (e.g., the development of post-fire debris flows) and informing the development of proactive management strategies. 

# Contents

* [**01_data_download**](https://github.com/jtkemper/burn-severity-co/blob/main/01_data_download.Rmd): this downloads the fire, vegetation, topography, etc. data that we need to train the models

* [**02_data_preprocessing**](https://github.com/jtkemper/burn-severity-co/blob/main/02_data_preprocessing.Rmd): clip dNBR data to fire boundary & mask water pixels, and soon – more!
