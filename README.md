# Coupon Service

Luxury tour of where Haskell is at from a production ready standpoint and so I can learn how to use Effects.

Effects: fused-effects (used by github for its semantics project)

Server: wai (performance on par with nginx)

Logging: fast-logger (logging that scales with multiple cores, supports files & rotation)

Migrations: postgresql-simple-migrations (temporary, I'd recommend something else like liquibase, but it works)

Persistence: postgresql-simple (other, more magical/productive options with better safety I didn't like as much, the simplicity of it's a win to me)
Testing: hspec (standard)

Automated formatting options: fourmolo, ormolu, floskell, stylish-haskell

Tooling: 
- GHCi
- haskell language server "just works"
- stack "just works" but sometimes a package isn't in the solved list

todo:
done - hspec
done - database
re-organize
tests
pick an automated formatter I GUESS

bumps:
- had to install libpq-dev so postgresql-simple would build (ubuntu)
- haskell language server climbed to ~5GB ram use over 12 hrs working on this
-- Strangely GHCi (the repl) uses only 80MB ram even after awhile using
