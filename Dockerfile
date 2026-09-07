FROM rocker/r-ver:4.6.1

RUN apt-get update && apt-get install -y \
    build-essential \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libicu-dev \
    libuv1-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('jsonlite','plumber'), repos='https://cloud.r-project.org')"

RUN R -q -e "stopifnot(requireNamespace('plumber', quietly=TRUE)); stopifnot(requireNamespace('jsonlite', quietly=TRUE))"

WORKDIR /app
COPY . /app

ENV PORT=8000
EXPOSE 8000

CMD ["R", "-q", "-e", "source('api.R'); pr <- plumber::pr(); pr <- plumber::pr_get(pr, '/health', health_handler); pr <- plumber::pr_post(pr, '/analyze', analyze_handler, parsers='json', serializer=plumber::serializer_unboxed_json()); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT','8000')))"]
