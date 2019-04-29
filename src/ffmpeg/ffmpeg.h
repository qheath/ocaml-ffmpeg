#include <libavformat/avformat.h>
#include <libavfilter/avfilter.h>
#include <libavutil/threadmessage.h>

#include "polymorphic_variant_values_stubs.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
//#include <caml/bigarray.h>
//#include <caml/threads.h>

#include "avutil_stubs.h"
#include "avcodec_stubs.h"


void print_error(const char *filename, int err);

void assert_empty_avoptions(AVDictionary *m);

void set_codec_option(AVDictionary **ptr_codec_opts, const char *key, const char *value, int is_output);
void set_format_option(AVDictionary **ptr_format_opts, const char *key, const char *value, int is_output);

extern const AVIOInterruptCB int_cb;
