FROM rocker/shiny:4.5.2

# System dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    libblas-dev \
    liblapack-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libfontconfig1-dev \
    libmagick++-dev \
    libudunits2-dev \
    libproj-dev \
    proj-data \
    proj-bin \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libsqlite3-dev \
    libuv1-dev \
    pandoc \
    tcl \
    tk \
    curl \
    libgsl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"

WORKDIR /srv/shiny-server

# Copy entire project (simpler and safer)
COPY . .

# Restore renv environment
RUN R -e "renv::restore(prompt = FALSE)"

# Install TinyTeX for PDF rendering
RUN R -e "install.packages('tinytex', repos='https://cloud.r-project.org/'); tinytex::install_tinytex(bundle='TinyTeX-2')"

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]