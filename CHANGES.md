Version 1.5.0 released 2014-11-22

* Fix support for EEP 18 empty objects `[{}]`
  https://github.com/etrepum/kvc/issues/8

Version 1.4.0 released 2014-05-20

* Erlang 17.0 compatibility
  https://github.com/etrepum/kvc/pull/6
* New kvc:path/3 API
  https://github.com/etrepum/kvc/pull/7

Version 1.3.1 released 2013-11-20

* Refactored tests to be compatible with R16
  https://github.com/etrepum/kvc/pull/5

Version 1.3.0 released 2013-01-08

* Added a LICENSE file (MIT)
* PropEr is now only a dependency when running the property based
  tests. `rebar -C rebar.proper.config get-deps compile && rebar -C
  rebar.proper.config eunit skip_deps=true`

Version 1.2.1 released 2012-01-03

* Added `registered` and `applications` parameters to .app for systools

Version 1.2 released 2011-08-29

* Added support for EEP 18 style `{}` and `{proplist()}`

Version 1.1.1 released 2011-06-15

* Fixed `kvc:to_proplist/1` edge cases, added better PropEr tests

Version 1.1.0 released 2011-06-11

* New `kvc:to_proplist/1`
* Added PropEr tests

Version 1.0.0 released 2011-02-22

* Initial release
