## v0.4.0

* Move the JavaScript package into its own opam package called `dispatch-js`,
  and rename the ocamlfind package from `dispatch.js` to `dispatch-js`.
  Also support `js_of_ocaml.3.0` while doing this.  This change allows the
  core package to build with fewer dependencies, and for your code to explicitly
  depend on the JavaScript library only if it is needed.
* Port build to [Jbuilder](https://github.com/janestreet/jbuilder).
* Test OCaml 4.04 in Travis, as well as add multi-distro testing.

## v0.3.0 2015-11-18

* Use pxx instead of camlp4 in `js_of_ocaml code`. (#9)
* Generate documentation for js and server libs. (#10, #11, #12)
* Modify route type to describe handlers. Should be reverse-compatible if the type itself isn't referenced. (#14)

## v0.2.0 2015-09-25

* Fix bug in parsing empty path strings, now parsed as [] (#5, @zoggy)
* Preserve the ordering of parameters in the association list (#8)
* Add support for `js_of_ocaml` (#4)
* Make route table map to a function that takes a parameter mapping and remaining path (#3, #6)

## v0.1.0 2015-09-14

* Initial public release
