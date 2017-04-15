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

void c_TMR_clearConfirmedParams (ReaderEtc *reader)
{
    if (!reader->destroyed &&
        reader->reader.readerType == TMR_READER_TYPE_SERIAL) {

        TMR_SR_SerialReader *sr;
        int i;

        sr = &reader->reader.u.serialReader;
        for (i = 0; i < TMR_PARAMWORDS; i++) {
            sr->paramConfirmed[i] = 0;
            sr->paramPresent[i] = 0;
        }
    }
}

TMR_Status c_TMR_destroy (ReaderEtc *reader)
{
    if (reader->destroyed) {
        return ERROR_ALREADY_DESTROYED;
    } else {
        free_all_transport_listeners (reader);
        reader->destroyed = true;
        return TMR_destroy (&reader->reader);
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
