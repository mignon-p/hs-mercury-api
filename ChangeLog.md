<!-- -*- text -*- (prevent Emacs from formatting ChangeLog incorrectly) -->

# Revision history for mercury-api

## 0.1.0.2  -- 2019-01-09

* Avoid including `xlocale.h` on Linux.  (Fixes [#1][1] and [#2][2].)

[1]: https://github.com/ppelleti/hs-mercury-api/issues/1
[2]: https://github.com/ppelleti/hs-mercury-api/issues/2

## 0.1.0.1  -- 2017-06-11

* Fixed a crash that could occur when peeking the mask of a TagFilter.  (I was reading the length as a 32-bit number instead of a 16-bit number, which could cause the length to be impossibly long.)

## 0.1.0.0  -- 2017-06-05

* First version. Released on an unsuspecting world.
