# Coupon Service

### An investigation into Haskell as a production language

github is using it for semantics
facebook is using it for spam

Effects: fused-effects (used by github for its semantics project)

Server: wai (performance on par with nginx)

Logging: fast-logger (logging that scales with multiple cores, supports files & rotation)

Migrations: postgresql-simple-migrations (temporary, I'd recommend something else like liquibase, but it works)

Persistence: postgresql-simple (other, more magical/productive options with better safety I didn't like as much, the simplicity of it's a win to me)
Testing: hspec (standard)

Automated formatting options: fourmolo, ormolu, floskell, stylish-haskell

future
better effect system
normal accessing of fields
new foundation
linear types
merger of haskell language servers

220 MiB used before bed

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
done - tests
done - handle closed rules
done - webserver
update log to use file (and log the time of msgs)
pick an automated formatter I GUESS

bumps:
- had to install libpq-dev so postgresql-simple would build (ubuntu)
- haskell language server climbed to ~5GB ram use over 12 hrs working on this
-- after another day it has climbed to ~10GB ram
-- another day of development, since I'm no longer adding so much it's held at max ~3Gb
-- Strangely GHCi (the repl) uses only 80MB ram even after awhile using
- had to make sure initdb was on my path in order to acquire a temp db which
  I'm using to run tests like a madman
  was in /usr/lib/postgresql/12/bin (still ubuntu)
-- had to create a swap on the VPS (it only has 2GB of ram) to build 
sudo mkdir -v /var/cache/swap
cd /var/cache/swap
sudo dd if=/dev/zero of=swapfile bs=1K count=4M
sudo chmod 600 swapfile
sudo mkswap swapfile
sudo swapon swapfile

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

example request:
{ 
  "items": [],
  "bundles": [],
  "codes": [],
  "time": "2021-05-21T04:16:19Z"
}
