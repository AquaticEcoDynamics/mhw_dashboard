# Marine Heatwave Dashboard

> [!NOTE]
> R version 4.3.1 (2023-06-16)

### Acknowledgement
This app is adapted from https://github.com/FACE-IT-project/WP1 by robert.schlegel@imev-mer.fr [shiny/demoMHW].

### Run the R Shiny App on VSCode Linux dev container
1. Install the following extensions:
   - Dev Containers
   - R
2. Reopen your VSCode in container.
3. Open a new terminal in your VSCode, type ```R```.
4. Run ```runApp("app_cockburnsound.R")```.
---
### Dockerised the R Shiny App (on Mac)
Run the following command in your laptop directory terminal accordingly:
1. To build your docker base (only need to be run once, except when you make changes in your Dockerfile_base)
   ```
   docker build -f Dockerfile_base --progress=plain -t mhw_base .
   ```
   Note: this will take around 30-40 minutes to run.
2. To build the R extension from docker file (run everytime you make changes on your code)
   ```
   docker build -f Dockerfile --progress=plain -t mhw:latest .
   ```
3. To run the app (run everytime you make changes on your code)
   ```
   docker run -p 3838:3838 mhw:latest
   ```
---
> [!IMPORTANT]
> Remember to change the R version in ```dockerfile``` to be same as the version of your R script (R studio).
---
### The dashboard
<p align="justify">The demoMHW R Shiny App, adapted from the original development by robert.schlegel@imev-mer.fr, is a tool for identifying Marine Heatwave Events (MHW) and introducing users with the MHW concept. It begins with the selection or upload of time series data, followed by customisation of key statistical parameters that will affect climatology and threshold. Users can then specify a temperature threshold and set the duration required for threshold exceedance to qualify as an MHW event. The app is organised into six-tab panels, including: </p>

<p align="justify">(a) an Overview section with a screenshot of Cockburn Sound virtual sensor location linked to temperature data points, and glossary explaining each label and legend in the figures; </p>

<p align="justify">(b) Time Series representation displaying raw data as a whole , broken down by month, year, Day of Year, and individual days; </p>

<p align="justify">(c) Statistics segment shows climatological and threshold data which is crucial for MHW determination; </p>

<p align="justify">(d) Exceedance analysis to identify consecutive days exceeding the specified threshold temperature; </p>

<p align="justify">(e) Detection module presenting detected MHW events through a tabulated format, lolli plot, and bubble plot within the context of the long-term dataset based on the base period; and </p>

<p align="justify">(f) The Main Event section highlighting the most significant MHW occurrence. </p>

Categorised lolli and main event plot can also be plotted.

> [!NOTE]
>  The uploaded file needs to have and consist only two columns as shown below:
>
>| t  | temp |
>| ------------- | ------------- |
>| date  | daily sea surface temperature  |
