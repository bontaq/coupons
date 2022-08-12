# Coupon Service Experience Report

I thought it was time to give Haskell a more thorough look for its production viability.

### Project
To keep it pretty short because this is mostly about what it's like to do work with Haskell, it's a Coupon Service that:
- accepts some [Context](https://github.com/bontaq/coupons/blob/main/src/Domain/Context.hs#L34)
- sees if the expression in the [Rule](https://github.com/bontaq/coupons/blob/main/src/Domain/Rule.hs) passes when applied to the Context
- returns the matching Code with Actions

The entrypoint is [Main](https://github.com/bontaq/coupons/blob/main/app/Main.hs) which brings up the server & db connection pool, parses the incoming data, and then calls the [Service](https://github.com/bontaq/coupons/blob/main/src/Service.hs).

### Workflow
Haskell has an extremely good Repl.  I would consider it the primary way to develop and interact with the code.  Typically, what you'll do a lot is 

 1. Have the code you want to test open
 2. Have the tests open
 3. Send the tests to the Repl (in Emacs, it's just `C-l`) 
 4. type `main` and hit enter to rerun the tests

That's the main loop for development.  As you're building though you'll also be using commands in the Repl like `:t` to look up the type of something, `:i` for information like Typeclass requirements, `:browse` to see what a module export, and `:r` to reload the current code.

If you encounter harder problems, two important things to know about are [Hoogle](https://hoogle.haskell.org/) and [Typed Holes](https://wiki.haskell.org/GHC/Typed_holes).  

Hoogle is a type-directed search for functions (though it does more, too).  If you're trying to solve something like, "How do I collect only the good values from this list of Maybes," all you have to do is search for `[Maybe a] -> [a]` and it comes back with your answer: `catMaybes`.  

You can also run Hoogle [locally](https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md) so that results include your code.  This gives a type-searchable and documented codebase.

Typed holes are a useful feature for feeling out what the types should be.  If you put a `_` in front of something, like `test = 1 + _something`, then on building or loading the code into the Repl, Haskell will give you information about what the type could be and information about all the surrounding types.

### Testing
There's 44 tests, on the first run they take about 10 seconds.  On the next run and after, about 5 seconds.  For both of those, it's mostly compiling time.  If there's no need to recompile, actually running the tests takes 0.047 seconds.  They all run in parallel.

The above numbers are from the slowest way to run the tests while working, by using the command line `stack test`.  The faster and preferred way is to run them in the Repl, which with since you're skipping compilation, takes less than 1 second to go from code change -> test results.

The tests for the [RuleRepository](https://github.com/bontaq/coupons/blob/main/src/Domain/RuleRepositorySpec.hs) run through both using a real database and a pure equivalent.  Higher level tests like in the [ServiceSpec](https://github.com/bontaq/coupons/blob/main/src/ServiceSpec.hs#L182) can choose to use the real database or pure equivalent.

### Problems
An algebraic effect handler is how you can get cool, good, things like easily swappable implementations to build up a meta-language for solving your problem.

I chose to use [Fused Effects](https://github.com/fused-effects/fused-effects) as the algebraic effect handler because it's fast and used / developed by Github for its [semantic](https://github.com/github/semantic) project.  If you ever wondered how Python in Github got jump to definition, that's that project.

As for why this section is called Problems, see [this](https://github.com/bontaq/coupons/blob/main/src/Domain/RuleRepository.hs#L95):

    instance (MonadIO m, Algebra sig m) => Algebra (RuleRepo :+: sig) (RuleRepoIO m) where
      alg handle sig ctx = RuleRepoIO $ do
 Surprisingly it's not that first line that causes pain.  That's just how it matches up you saying "This function needs a `RuleRepo` and this is the interpreter for `RuleRepoIO` if you run it with that" with the real code.
 
 The second line is painful lack of documentation.  It took me hours to get all the types to line up correctly and actually return something when used.  If you look at the [Logging Effect](https://github.com/bontaq/coupons/blob/main/src/Effects/Logging.hs) definition, you can see another instance of trouble: all I wanted to do was pass in a logger, but that broke automatic derivation.  I should be using a Reader effect for configuration like that, but I didn't know.

Even with the bad experience, what it provides is extremely nice for actually using the effects, like this:

    getActions ::
      ( Has Log sig m
      , Has RuleRepo sig m
      ) => Context -> m (Either RepoError [(Code, [Action])])
    getActions context = do
      openRules <- getOpenRules
      log "hello"
      
Pretty much dependency injection, but you can do more with it.  No reason you couldn't have some Exception and Telemetry effects as well, and rewrite the `RuleRepo` effect to be in terms of more generic Database / Store effects.

There are much more humane (human?) effect libraries out there like [Polysemy](https://hackage.haskell.org/package/polysemy) or [Eff](https://github.com/hasura/eff) or [Simple-Effects](https://hackage.haskell.org/package/simple-effects) but I wanted speed.  I wanted to include an example of where Haskell can get difficult.

### Great Things
* Haskell Language Server has HLint built in (it's a linter) and it made me better at Haskell.  Its suggestions are great, like "replace `catMaybes . fmap` with `mapMaybe`".  I didn't even know the fn existed.
* Parallel tests with a real database run in less than a second.
* Implementing a database connection pool took 20 minutes
* Doing the whole "use a real DB" thing in tests wasn't hard and seeing in the [HSpec](https://hspec.github.io/writing-specs.html) docs, in parentheses "(for example, if you wanted to open a database connection before each item and pass the connection in)", literally exactly what I was trying to do, was refreshing.

### The Future
Haskell's having a bit of a moment in that development speed has picked up, long standing problems are being fixed, and it feels like it's all coming together.  Here's what's happened recently:

[Linear types](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html) were introduced in GHC 9 which allow the expression of single-use resources ala rust.

Previously there were two projects implementing the language server protocol for Haskell: ghcide and haskell-ide-engine.  They've joined forces to build one excellent [Haskell Language Server](https://github.com/haskell/haskell-language-server) which hit their 1.0 release in February.  New features are coming quickly but ram use is high (~5Gb after a day of work if you are doing a lot).  It also has excellent support in VSCode so no, not everyone has to learn emacs.

Records in Haskell were infamously difficult due to various name collisions and almost requiring the use of [Lenses](https://kseo.github.io/posts/2016-12-10-encodings-of-lense.html) to access and update records.  As of [GHC 9.2](https://www.haskell.org/ghc/blog/20210401-ghc-9.2.1-alpha1-released.html) (currently in alpha) this is no longer the case and record access has been normalized.  `let name = person.name` away, or even `names = fmap .name persons` if you're feeling fancy.

You know how I chose Fused Effects over a more sane effects system in the name of speed?  Well a [GHC Proposal](https://github.com/ghc-proposals/ghc-proposals/pull/313) to make the nice effect systems fast was accepted and is being implemented.

Haskell finally got a [real foundation](http://haskell.foundation/) working to increase adoption, chaired by simon peyton jones.

[GHC 9.2](https://www.haskell.org/ghc/blog/20210401-ghc-9.2.1-alpha1-released.html) continues its gifts of making Haskell vastly more approachable by also introducing the `{-# LANGUAGE GHC2021 #-}` pragma.  Because almost every feature in Haskell is a flag, that's why you see about 50 `{-#  LANGUAGE blahblah #-}` at the top of most files.  That freaks everyone out, so this meta-flag was introduced that turns on everything we'd consider standard to writing Haskell in 2021.

### Overall

I was a bit afraid to try this project because you know, what if Haskell actually isn't good and I only like doing fun things with it instead of real work with lots of tests.  Instead I've found it's extremely productive with a fast feedback loop just like that [1994 study for the Navy](http://web.cecs.pdx.edu/~apt/cs457_2005/hudak-jones.pdf) found it -- even though then it was against C++ and Ada.

Really leaning into Repl driven development lead to an interesting thing:  the [Helpers](https://github.com/bontaq/coupons/tree/main/src/Helpers) modules.  Both of those are expected to be used only in the Repl to quickly get information into the Database -- I can imagine a world where most files include some Repl helpers, for easier interaction with code.

### Performance

To make sure performance was consistent and to make sure there were no memory leaks, I've been hitting the service with 1000 requests (over 10 seconds) every minute for 8 hours.  It's running on an 18$/m VPS with 2Gb ram and 2VCpus.  

Response times in msec:

min: 26

max: 335

median: 46

p95: 98

p99: 143

Cpu: Never above 15%

Memory: Total used was at 220MiB when I began, it's at 230MiB now.  That's for the whole system including postgres, the Haskell server has stayed at 60MiB. 

Overall: Cool as a cucumber and could be pushed a lot more.  I'm not even doing anything in parallel or using caching.

### Ending
Thanks!

P.S. Yes Haskell has an OK library for GRPC but I'd need to test it.  Facebook released an officially supported version of their own RPC framework for [Haskell](https://engineering.fb.com/2021/02/05/open-source/hsthrift/).  Surprising nobody at all, Haskell also has a very [cool library](https://github.com/morpheusgraphql/morpheus-graphql) for building GraphQL services.  

P.P.S. Are you mad at reading code backwards because composition "just works like that" and Hasklars are mean / trying to goof up your eyeballs* / decided reading backwards is cool?  We can define our own pipe operator which I think actually worked great in the mob tool and it's a good idea. `toUpperCase . show` vs `show |> toUpperCase`.

### Notes from development

#### Libraries:
Effects: fused-effects (used by github for its semantics project)

Server: Scotty, Haskell's version of Sinatra, but powered by Wai (performance on par with nginx)

Logging: fast-logger (logging that scales with multiple cores, supports files & rotation)

Migrations: postgresql-simple-migrations (temporary, I'd recommend something else like liquibase, but it works)

Persistence: postgresql-simple (other, more magical/productive options with better safety I didn't like as much but probably should use, the simplicity of it's a win to me)

Testing: hspec (standard)

#### Things I haven't got to:
Enabling -Wall to fix all the warnings

Using a Reader to pass in the logger to the Logger effect

Automated formatting options: fourmolo, ormolu, floskell, stylish-haskell

Criterion for built in speed testing

Property testing

Using parallel map for the rules

Dockerized

Github action that runs tests on PRs

#### Cruft:
220 MiB used before bed

example failed test output:

Failures:

  /home/ian/code/coupons/src/Domain/RuleRepositorySpec.hs:83:9: 
  1) RuleRepoState.addRule Stores a new closed rule
       expected: RuleState {rules = [], closedRules = []}
        but got: RuleState {rules = [Rule (HasCode "test-99" (Name "test")) []], closedRules = []}

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
-- had to increase files open limit
ulimit -n 10000
-- had to increas file size limit
ulimit -f 10000

\* ehehehe

example request:
{ 
  "items": [],
  "bundles": [],
  "codes": [],
  "time": "2021-05-21T04:16:19Z"
}

