FROM rocker/shiny:latest

# Environment variables
ENV TZ="Etc/UTC"
ENV CRAN="https://cloud.r-project.org"
ENV LANG=en_US.UTF-8

# Install system dependencies, curl, gpg, and unixodbc drivers
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    gnupg \
    unixodbc \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libmariadb-dev \
    libmariadb-dev-compat \
    libzstd-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

# Register Microsoft GPG key and repository with explicit signed-by pointing
RUN curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor -o /usr/share/keyrings/microsoft-prod.gpg \
    && echo "deb [arch=amd64,arm64,armhf signed-by=/usr/share/keyrings/microsoft-prod.gpg] https://packages.microsoft.com/ubuntu/22.04/prod jammy main" > /etc/apt/sources.list.d/mssql-release.list \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y msodbcsql18 \
    && rm -rf /var/lib/apt/lists/*

# Consolidate R package installations for faster builds and smaller layers
RUN R -e "install.packages(c('xfun', 'shiny', 'pak', 'tidyr', 'lubridate', 'bslib', 'ggtext'), repos='https://cloud.r-project.org')" && \
    R -e "pak::pkg_install(c('RMariaDB', 'RJDBC', 'ggfx', 'patchwork', 'emojifont', 'forcats', 'here', 'targets', 'htmltools', 'dbplyr', 'ggh4x', 'ggiraph', 'shinyWidgets', 'odbc', 'RMySQL'))"

# IMPORTANT: Copy your app files into the container FIRST
COPY ./shinyApp /srv/shiny-server

# Allow the shiny user to write cache files in the app directory
RUN chown -R shiny:shiny /srv/shiny-server

# Allow the shiny user to read installed R packages
RUN chmod -R 755 /usr/local/lib/R/site-library