#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <HsFFI.h>

#include "glue.h"
#include "serial_reader_imp.h"

bool isSecureAccessEnabled;

static TMR_Status free_transport_listener (TMR_Reader *reader,
                                           TransportListenerEtc *listener,
                                           TransportListenerEtc **prev)
{
    TMR_Status stat;

    stat = TMR_removeTransportListener (reader, &listener->block);
    free (listener->key);
    hs_free_fun_ptr (listener->funPtr);
    *prev = listener->next;
    free (listener);

    return stat;
}

static void free_all_transport_listeners (ReaderEtc *reader)
{
    while (reader->transportListeners) {
        free_transport_listener (&reader->reader,
                                 reader->transportListeners,
                                 &reader->transportListeners);
    }
}

TMR_Status c_TMR_create (ReaderEtc *reader, const char *deviceUri)
{
    reader->transportListeners = NULL;
    reader->readPlan = NULL;
    reader->destroyed = false;
    return TMR_create (&reader->reader, deviceUri);
}

TMR_Status c_TMR_connect (ReaderEtc *reader)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else
        return TMR_connect (&reader->reader);
}

TMR_Status c_TMR_destroy (ReaderEtc *reader)
{
    if (reader->destroyed) {
        return ERROR_ALREADY_DESTROYED;
    } else {
        TMR_Status status;

        free_all_transport_listeners (reader);
        status = TMR_destroy (&reader->reader);
        free (reader->readPlan);
        reader->destroyed = true;
        return status;
    }
}

TMR_Status c_TMR_read (ReaderEtc *reader, uint32_t timeoutMs, int32_t *tagCount)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else
        return TMR_read (&reader->reader, timeoutMs, tagCount);
}

TMR_Status c_TMR_hasMoreTags (ReaderEtc *reader)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else
        return TMR_hasMoreTags (&reader->reader);
}

TMR_Status c_TMR_getNextTag (ReaderEtc *reader, TMR_TagReadData *tagData)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else
        return TMR_getNextTag (&reader->reader, tagData);
}

TMR_Status c_TMR_executeTagOp (ReaderEtc *reader,
                               TMR_TagOp *tagop,
                               TMR_TagFilter *filter,
                               TMR_uint8List *data)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else
        return TMR_executeTagOp (&reader->reader, tagop, filter, data);
}

TMR_Status c_TMR_paramSet(ReaderEtc *reader,
                          TMR_Param key,
                          const void *value)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else {
        TMR_Status status;

        status = TMR_paramSet (&reader->reader, key, value);
        if (key == TMR_PARAM_READ_PLAN) {
            if (status == TMR_SUCCESS) {
                /* TMR_paramSet appears to only do a shallow copy of the
                 * read plan, meaning that the internal pointers need to
                 * stay pointing at valid memory.  We handle this by
                 * keeping ownership of the ReadPlanEtc, which includes
                 * space for the lists to point at.
                 *
                 * When the read plan is set, we need to free the old
                 * ReadPlanEtc, and keep a pointer to the new one.
                 */
                free (reader->readPlan);
                reader->readPlan = (ReadPlanEtc *) value;
            } else {
                /* If the set fails, we need to free the new ReadPlanEtc. */
                free ((void*) value);
            }
        }

        return status;
    }
}

TMR_Status c_TMR_paramGet(ReaderEtc *reader, TMR_Param key, void *value)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else {
        /* TMR_PARAM_READ_PLAN appears to be write-only, so fulfill the
         * read from our cached copy. */
        if (key == TMR_PARAM_READ_PLAN) {
            TMR_ReadPlan *rp;

            rp = (TMR_ReadPlan *) value;
            if (reader->readPlan) {
                /* Shallow copy the ReadPlan, although the pointers will
                 * still point at parts of the cached ReadPlanEtc. */
                *rp = reader->readPlan->plan;
            } else {
                /* No cached read plan, which means it's initialized to
                 * the default, which is this: */
                TMR_RP_init_simple (rp, 0, NULL, TMR_TAG_PROTOCOL_GEN2, 1);
            }

            return TMR_SUCCESS;
        } else {
            return TMR_paramGet (&reader->reader, key, value);
        }
    }
}

TMR_Status c_TMR_paramList (ReaderEtc *reader, TMR_Param *keys, uint32_t *len)
{
    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;
    else
        return TMR_paramList (&reader->reader, keys, len);
}

/* takes ownership of func and unique */
TMR_Status c_TMR_addTransportListener (ReaderEtc *reader,
                                       TMR_TransportListener func,
                                       char *unique)
{
    TransportListenerEtc *listener;

    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;

    listener = (TransportListenerEtc *) malloc (sizeof (TransportListenerEtc));
    if (! listener)
        return TMR_ERROR_OUT_OF_MEMORY;

    listener->block.listener = func;
    listener->block.cookie = NULL;
    listener->key = unique;
    listener->funPtr = (HsFunPtr) func;
    listener->next = reader->transportListeners;
    reader->transportListeners = listener;

    return TMR_addTransportListener (&reader->reader, &listener->block);
}

TMR_Status c_TMR_removeTransportListener (ReaderEtc *reader,
                                          const char *unique)
{
    TransportListenerEtc *listener, **prev;

    if (reader->destroyed)
        return ERROR_ALREADY_DESTROYED;

    prev = &reader->transportListeners;
    for (listener = reader->transportListeners;
         listener != NULL;
         prev = &listener->next, listener = listener->next) {
        if (0 == strcmp (unique, listener->key))
            return free_transport_listener (&reader->reader, listener, prev);
    }

    return TMR_ERROR_INVALID;
}

const char *c_TMR_strerr (ReaderEtc *reader, TMR_Status status)
{
    TMR_Reader *tmr;

    if (reader && !reader->destroyed)
        tmr = &reader->reader;
    else
        tmr = NULL;

    if (status == ERROR_ALREADY_DESTROYED)
        return "Attempt to use reader after it was destroyed";
    else
        return TMR_strerr (tmr, status);
}
