FROM haskell:8.10-buster
WORKDIR /
COPY . .

RUN apt-get update

# needed for postgresql-simple requirements
RUN apt-get install -y libpq-dev

RUN stack build
