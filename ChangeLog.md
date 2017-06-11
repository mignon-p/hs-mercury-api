<!-- -*- text -*- prevent Emacs from formatting ChangeLog incorrectly -->
# Revision history for mercury-api

## 0.1.0.1  -- 2017-06-11

* Fixed a crash that could occur when peeking the mask of a TagFilter.  (I was rading the length as a 32-bit number instead of a 16-bit number, which could cause the length to be impossibly long.)

## 0.1.0.0  -- 2017-06-05

* First version. Released on an unsuspecting world.
