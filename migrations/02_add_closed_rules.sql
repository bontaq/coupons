create table if not exists closed_rules(
  id serial primary key,
  expression JSONB,
  result JSONB,
  code varchar(255)
);

create index on closed_rules (code);
