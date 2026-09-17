FROM dps_forecast_v1

RUN apt-get update && apt-get install -y libcurl4-openssl-dev libxml2-dev libzstd-dev
RUN apt update && apt -y install cron

ENV CRAN="https://cloud.r-project.org"
ENV LANG=en_US.UTF-8

# Install system dependencies for openssl and textshaping
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libzstd-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    default-libmysqlclient-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libfontconfig1-dev

RUN R -e "install.packages(c('pak', 'tidyr', 'lubridate',  'bslib', 'ggtext'), repos='https://cloud.r-project.org', dependencies = TRUE, verbose = TRUE)"
RUN R -e "pak::pkg_install('RMySQL')"
RUN R -e "pak::pkg_install('RJDBC')"
RUN R -e "pak::pkg_install('lme4')"
RUN R -e "pak::pkg_install('lmerTest')"



# Copy the scripts
COPY code_data.R /root/code_data.R

# Copy hello-cron file to the cron.d directory
COPY crontab /etc/cron.d/crontab

# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/crontab

# Apply cron job
RUN crontab /etc/cron.d/crontab

# Run the command on container startup
CMD printenv > /etc/environment && cron -f