These are patches to be applied to Mercury API to fix various bugs.
I've submitted all of them (except `config.patch`) to ThingMagic for
possible inclusion in a future release.

`config.patch` - This increases the maximum length of URIs from 64 bytes to 256 bytes.  Unlike all the other patches, this is a configuration change, not a bug fix, so it has not been submitted upstream.

`eintr.patch` - This retries the `select()` operation when it returns `EINTR`.  This is always good practice, but it seemed to be necessary in my situation to avoid failing with a timeout.  There are probably plenty of other places in the API where this should be done, too.

`extern.patch` - This fixes a problem I ran into with `isSecureAccessEnabled` being multiply defined, when compiling with cabal.  (Strangely, doesn't happen when using ThingMagic's Makefile.)

`host-c-library.patch` - The build failed when `TMR_USE_HOST_C_LIBRARY` was defined.  This fixes that.

`infinite-loop.patch` - This avoids an infinite loop that I ran into, when somehow `readOffSet` was 142, and `dataLength` was 128.  Not sure how that happened, and it probably indicates another bug, but at least this fix prevents going into an infinite loop in that situation.

`metadataflag-size.patch` - `paramSet()` treated `TMR_PARAM_METADATAFLAG` as `TMR_TRD_MetadataFlag`, while `paramGet()` treated `TMR_PARAM_METADATAFLAG` as `uint16_t`.  Since `sizeof(TMR_TRD_MetadataFlag)` is 4 but `sizeof(uint16_t)` is 2, the sizes didn't match.  This patch fixes `paramGet()` to also treat it as `TMR_TRD_MetadataFlag`.

`mingw-snprintf.patch` - Fixes a problem I ran into on MinGW, where `sprintf_s()` doesn't seem to work properly, but `snprintf()` does work properly.

`typos.patch` - This just fixes a bunch of small typos I found.

`windows-time.patch` - On Windows, `tmr_gettime_low()` and `tmr_gettime_high()` were returning the number of 100-nanosecond units since 1/1/1601, rather than the number of milliseconds since 1/1/1970.  This caused the `timestampLow` and `timestampHigh` fields of `TMR_TagReadData` to be populated incorrectly.  This patch fixes that problem.
