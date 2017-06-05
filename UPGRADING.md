Hopefully I will keep this package up-to-date with the latest version
of Mercury API.  But if you're in the position of needing to update
Mercury API, follow these instructions:

* Unzip the Mercury API zipfile.

* Copy the files from `c/src/api/` in the Mercury API distribution to
  `cbits/api/` in this package.

* Run `dos2unix` on the files you copied, to normalize the line
  endings.  (In the Mercury API distribution, they are a random
  combination of DOS and Unix line endings, often in the same file.)

* Apply the patches from the `cbits/api/patches/` directory.

* Update `mercury-api.cabal` with any additional C files that were
  added in the new release.  Note that not all C files are needed; in
  particular, `serial_transport_tcp_posix.c` and
  `tmr_loadsave_configuration.c` are not needed.  Also, note that the
  files are supposed to be listed in [dependency order][1].

* Run `util/generate-tmr-hsc.pl` (no arguments needed), which will
  update the generated files `Enums.hsc` and `Params.hs` based on the
  new `.h` and `.c` files.  (In particular, this will automatically
  pick up new status codes and new parameters.)

[1]: https://ghc.haskell.org/trac/ghc/ticket/13786#comment:3
