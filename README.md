# Pretty
* A not intrusive pretty print tool

# quick start
* Add to rebar.config
```erlang
{deps, [
  ...
  {pretty, {git, "https://github.com/QCute/pretty.git", {branch, "master"}}}
]}.
```

* Usage 
```erlang
%% setup
code:ensure_loaded(pretty).
%% type
[0, true, undefined, a, [b], {c}, #{d => e}, <<"f">>].
```
