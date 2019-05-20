# Fish die-offs are concurrent with thermal extremes in north temperate lakes

### Authors
Aaron Till, Andrew Rypel, Andrew Bray, and Sam Fey.

### Abstract
As environmental temperatures continue to rise and organisms experience novel and potentially lethal conditions, the possibility of increased mass mortality events for animal populations appears likely. Yet due to die-off rarity and unpredictability, there has been few largescale attempts to quantify the relationship between mass die-offs and local environmental temperatures. Here, we address this issue by analyzing a database of 502 freshwater fish die-offs combined with lake-specific temperature profiles simulated for north temperate lake ecosystems. Die-offs driven by extreme summer conditions occurred disproportionately in lakes with warmer average surface temperatures and during periods of extreme heat. In contrast, we observe no relationships between current thermal extremes and die-offs attributed to infectious disease or winter environmental conditions. We forecast summer fish die-offs for north temperate lakes to double by mid-century (2041-2059) and increase more than four-fold by late century (2081-2099), particularly at southern latitudes. These results expose a direct link between novel temperature regimes and the increased probability of catastrophic ecological events in freshwater ecosystems.

### Compendium organization
The code to perform the analysis found the manuscript is organized into a series of R Markdown files.

1. `tidy_and_clean_data.Rmd`: Reads in raw files, cleans and merges them.
2. `prepare_data_for_modeling.Rmd`: Partitions and scales historical data.
3. `fit_models_for_assessment.Rmd`: Fit series of models in train/test framework.
4. `assess_models.Rmd`: Assess out-of-sample performance and stability of coefficients.
5. `fit_full_models.Rmd`: Fit subset of models on full historical data.

Each of the figures in the manuscript is created by an R script in the `/figures` subdirectory.

### Data
The data set for this project fall into three categories: raw data, processed data, and model data. All three are available in the "releases" associated with this repository. You can either download the large zip file containing all of the data via github, or use R to download it directly into a repository that has been locally cloned.

```
# install.packages("piggyback")
# require(piggyback)
pb_download(repo = "andrewpbray/fish_MMEs_and_thermal_extremes", 
            tag  = "v0.0.1",
            dest = "data")
```
