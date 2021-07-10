FROM haskell:8.10
WORKDIR /
COPY . .

# since pg_config is required
RUN apt-get install -y postgresql
# also needed for postgresql-simple requirements
RUN apt-get install -y libpq-dev

RUN stack build
