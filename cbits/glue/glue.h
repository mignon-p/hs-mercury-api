#ifndef GLUE_H
#define GLUE_H

#include <stdbool.h>
#include "tm_reader.h"

#define ERROR_TYPE_BINDING 255L

#define ERROR_BINDING(x)                TMR_STATUS_MAKE(ERROR_TYPE_BINDING, (x))

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

#endif  /* GLUE_H */
