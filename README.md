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
4. Run ```runApp("app_cockburn.R")```.
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
