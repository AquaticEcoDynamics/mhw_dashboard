### Dockerised the R Shiny App (on Mac)
Run the following command in your directory terminal accordingly:
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