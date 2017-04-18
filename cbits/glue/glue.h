#ifndef GLUE_H
#define GLUE_H

#include <stdbool.h>
#include <HsFFI.h>

#include "tm_reader.h"

/** An error which originates from the Haskell binding, not the underlying C library. */
#define ERROR_TYPE_BINDING 127L

#define ERROR_BINDING(x)                TMR_STATUS_MAKE(ERROR_TYPE_BINDING, (x))

/** Attempt to use reader after it was destroyed. */
#define ERROR_ALREADY_DESTROYED         ERROR_BINDING(1)

typedef struct TransportListenerEtc {
    TMR_TransportListenerBlock block;
    char *key;
    HsFunPtr funPtr;
    struct TransportListenerEtc *next;
} TransportListenerEtc;

typedef struct ReaderEtc {
    TMR_Reader reader;
    TransportListenerEtc *transportListeners;
    bool destroyed;
} ReaderEtc;

TMR_Status c_TMR_create (ReaderEtc *reader, const char *deviceUri);
TMR_Status c_TMR_connect (ReaderEtc *reader);
TMR_Status c_TMR_destroy (ReaderEtc *reader);
TMR_Status c_TMR_read (ReaderEtc *reader, uint32_t timeoutMs, int32_t *tagCount);
TMR_Status c_TMR_hasMoreTags (ReaderEtc *reader);
TMR_Status c_TMR_getNextTag (ReaderEtc *reader, TMR_TagReadData *tagData);
TMR_Status c_TMR_executeTagOp (ReaderEtc *reader,
                               TMR_TagOp *tagop,
                               TMR_TagFilter *filter,
                               TMR_uint8List *data);
TMR_Status c_TMR_paramSet(ReaderEtc *reader,
                          TMR_Param key,
                          const void *value);
TMR_Status c_TMR_paramGet(ReaderEtc *reader, TMR_Param key, void *value);
TMR_Status c_TMR_paramList (ReaderEtc *reader, TMR_Param *keys, uint32_t *len);
TMR_Status c_TMR_addTransportListener (ReaderEtc *reader,
                                       TMR_TransportListener func,
                                       char *unique);
TMR_Status c_TMR_removeTransportListener (ReaderEtc *reader,
                                          const char *unique);
const char *c_TMR_strerr (ReaderEtc *reader, TMR_Status status);

#endif  /* GLUE_H */
