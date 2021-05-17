CREATE TABLE if not exists rules(
  id SERIAL PRIMARY KEY,
  expression JSONB,
  result JSONB
);
