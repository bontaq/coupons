# Coupon Service

A tour of where Haskell is at from a production ready standpoint

Effects: fused-effects (used by github for its semantics project)

Server: wai (performance on par with nginx)

Logging: fast-logger (logging that scales with multiple cores, supports files & rotation)

Migrations: postgresql-simple-migrations (temporary, I'd recommend something else like liquibase, but it works)

Persistence: postgresql-simple (other, more magical/productive options with better safety I didn't like as much, the simplicity of it's a win to me)
Testing: hspec (standard)

Automated formatting options: fourmolo, ormolu, floskell, stylish-haskell

example failed test output:

Failures:

  /home/ian/code/coupons/src/Domain/RuleRepositorySpec.hs:83:9: 
  1) RuleRepoState.addRule Stores a new closed rule
       expected: RuleState {rules = [], closedRules = []}
        but got: RuleState {rules = [Rule (HasCode "sakib42" (Name "test")) []], closedRules = []}

  To rerun use: --match "/RuleRepoState/addRule/Stores a new closed rule/"

Randomized with seed 1465572638

Finished in 0.0407 seconds

Tooling: 
- GHCi
- haskell language server "just works"
- stack "just works" but sometimes a package isn't in the solved list

todo:
done - hspec
done - database
done - re-organize
tests
update log to use file (and log the time of msgs)
pick an automated formatter I GUESS

bumps:
- had to install libpq-dev so postgresql-simple would build (ubuntu)
- haskell language server climbed to ~5GB ram use over 12 hrs working on this
-- after another day it has climbed to ~10GB ram
-- Strangely GHCi (the repl) uses only 80MB ram even after awhile using
- had to make sure initdb was on my path in order to acquire a temp db which
  I'm using to run tests like a madman
  was in /usr/lib/postgresql/12/bin (still ubuntu)

good:
- haskell language server has very good built in warnings via hlint
-- for example, I had "catMaybes . fmap (evalRule cart)"
-- and it suggested
-- mapMaybe (evalRule cart)
-- certainly good enough suggestions to make me seem like a better haskell programmer

wonder:
- parallel tests with a real database run in less than a second
- build and running tests takes < 5 s
  with more tests, full build and test takes 5.2s
  with --fast, still less than 5s
