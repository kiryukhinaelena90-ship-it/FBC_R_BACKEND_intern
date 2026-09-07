FROM rocker/r-ver:4.6.1

RUN R -q -e "install.packages(c('plumber','jsonlite'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

ENV PORT=8000
EXPOSE 8000

CMD ["R", "-q", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT','8000')))"]
