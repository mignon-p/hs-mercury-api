#ifndef GLUE_H
#define GLUE_H

#include <stdbool.h>
#include <time.h>
#include <HsFFI.h>

#include "tm_reader.h"

/** An error which originates from the Haskell binding, not the underlying C library. */
#define ERROR_TYPE_BINDING 127L

#define ERROR_BINDING(x)                TMR_STATUS_MAKE(ERROR_TYPE_BINDING, (x))

/** Attempt to use reader after it was destroyed. */
#define ERROR_ALREADY_DESTROYED         ERROR_BINDING(1)

typedef struct TransportListenerEtc TransportListenerEtc;
typedef struct ReaderEtc ReaderEtc;
typedef struct List16 List16;
typedef struct List8 List8;
typedef struct ReadPlanEtc ReadPlanEtc;
typedef struct TagOpEtc TagOpEtc;
typedef struct TagFilterEtc TagFilterEtc;

struct ReaderEtc {
    TMR_Reader reader;
    TransportListenerEtc *transportListeners;
    ReadPlanEtc *readPlan;
    bool destroyed;
};

struct TransportListenerEtc {
    TMR_TransportListenerBlock block;
    char *key;
    HsFunPtr funPtr;
    struct TransportListenerEtc *next;
};

/* Has the same storage layout as the various TMR_*List structs
 * which use a 16-bit length, such as:
 * TMR_uint8List
 * TMR_uint16List
 * TMR_uint32List
 * TMR_StatsPerAntennaValuesList
 * Can also be used for TMR_String, if you ignore len.
 */
struct List16 {
    /** The array of values */
    void *list;
    /** The number of entries there is space for in the array */
    uint16_t max;
    /** The number of entries in the list - may be larger than max, indicating truncated data. */
    uint16_t len;
};

/* Has the same storage layout as the various TMR_*List structs
 * which use an 8-bit length, such as:
 * TMR_int8List
 * TMR_RegionList
 * TMR_TagProtocolList
 */
struct List8 {
    /** The array of values */
    void *list;
    /** The number of entries there is space for in the array */
    uint8_t max;
    /** The number of entries in the list - may be larger than max, indicating truncated data. */
    uint8_t len;
};

#define GLUE_MAX_MASK TMR_MAX_EMBEDDED_DATA_LENGTH

struct TagFilterEtc {
    TMR_TagFilter filter;
    uint8_t mask[GLUE_MAX_MASK];
};

#define GLUE_MAX_DATA16 TMR_MAX_EMBEDDED_DATA_LENGTH

struct TagOpEtc {
    TMR_TagOp tagop;
    TMR_TagData epc;
    uint16_t data16[GLUE_MAX_DATA16];
};

#define GLUE_MAX_ANTENNAS TMR_SR_MAX_ANTENNA_PORTS
#define GLUE_MAX_GPIPORTS 16

struct ReadPlanEtc {
    TMR_ReadPlan plan;
    uint8_t antennas[GLUE_MAX_ANTENNAS];
    uint8_t gpiPorts[GLUE_MAX_GPIPORTS];
    TagFilterEtc filter;
    TagOpEtc tagop;
};

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
TMR_Status c_TMR_firmwareLoad (ReaderEtc *reader,
                               uint8_t *firmwareStart,
                               uint32_t firmwareSize);
TMR_Status c_TMR_paramSet(ReaderEtc *reader,
                          TMR_Param key,
                          const void *value);
TMR_Status c_TMR_paramGet(ReaderEtc *reader, TMR_Param key, void *value);
void c_default_read_plan (TMR_ReadPlan *rp);
TMR_Status c_TMR_paramList (ReaderEtc *reader, TMR_Param *keys, uint32_t *len);
TMR_Status c_TMR_addTransportListener (ReaderEtc *reader,
                                       TMR_TransportListener func,
                                       char *unique);
TMR_Status c_TMR_removeTransportListener (ReaderEtc *reader,
                                          const char *unique);
const char *c_TMR_strerr (ReaderEtc *reader, TMR_Status status);
void *c_new_c_locale (void);
int c_format_time (char *buf,
                   size_t len,
                   const char *fmt,
                   time_t seconds,
                   bool local,
                   void *locale);

#endif  /* GLUE_H */
