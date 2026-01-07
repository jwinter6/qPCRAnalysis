# Get Base
FROM rocker/shiny-verse

# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    libcairo2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*


# install R
RUN R -e "install.packages(c('shinydashboard', 'DT', 'DBI', 'RSQLite', 'digest', 'readxl', 'readr', 'shinyFiles', 'tools', 'lubridate', 'ggplot2', 'plotly', 'shinydashboardPlus', 'ggthemes', 'writexl', 'outliers', 'EnvStats', 'pool', 'bslib', 'fresh', 'rmarkdown', 'knitr', 'tinytex'))"

# Install TinyTeX for PDF report export
RUN R -e "tinytex::install_tinytex()"

			
# Copy the Shiny app code
COPY app.R /app/
COPY /R /app/R/

# Expose the application port
EXPOSE 3901

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3901)"]
