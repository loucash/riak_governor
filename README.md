# riak_governor

[![Build Status](https://travis-ci.org/loucash/riak_governor.svg?branch=master)](https://travis-ci.org/loucash/riak_governor)

"What's an ensemble of leaders? A riak_governor. Because there can be only one redundant source of truth"
@darach

riak_governor is an extension to riak_core that provides a simple api to check a
leader of the unique set of preflist owning nodes. It uses rafter or
riak_ensemble underneath for leader election. This work is **experimental**.

When application starts and on each ring change event riak_governor determines the set of
ensembles. There is one ensemble of each unique set of proflist owning nodes.
Each ensemble is represented by rafter process on each node that belongs to
that ensemble.
An ensemble (a group of nodes) elects a leader using [raft consensus
protocol](https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf).

When `riak_governor:is_leader/1,2` is called, preflist is created, then
unique nodes from this preflist are extracted and apprioriate ensemble is asked
about a leader.

## How to build it
`make deps compile`

## Tutorial

```erlang
(dev1@127.0.0.1)9> Key = {<<"key1">>, <<"key2">>}.
(dev1@127.0.0.1)10> riak_core_apl:get_primary_apl(riak_core_util:chash_key(Key), 3, my_service).
[{{1216015034185477818661659645970969856198100123648,
   'dev2@127.0.0.1'},
  primary},
 {{1221724024956301658185892789848767836743631110144,
   'dev3@127.0.0.1'},
  primary},
 {{1227433015727125497710125933726565817289162096640,
   'dev4@127.0.0.1'},
  primary}]
(dev1@127.0.0.1)11> riak_governor:is_leader(node(), Key).
false
(dev1@127.0.0.1)12> riak_governor:is_leader('dev2@127.0.0.1', Key).
true
(dev1@127.0.0.1)13> riak_governor:is_leader('dev3@127.0.0.1', Key).
false
(dev1@127.0.0.1)14> riak_governor:is_leader('dev4@127.0.0.1', Key).
false

```

## Contributing

If you see something missing or incorrect, do not hesitate to create an issue
or pull request. Thank you!

## Roadmap
- Add tests
- Add API to inspect ensembles

## Changelog

- 0.1.0: Initial Release

## Authors

- Lukasz Biedrycki / @loucash: initial implementation
- Darach Ennis / @darach: came up with the name, implementation of pluggable
  consensus providers
- Konrad Sosnowski / @konrads: working hard to make riak_ensemble plugin working
- Heinz N. Gies / @Licenser: riak_governor [tests](https://github.com/Licenser/governor_test)
