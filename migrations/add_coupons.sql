CREATE TABLE if not exists rule(
  id int,
  expression JSONB,
  result JSONB,
  PRIMARY KEY (id)
);
