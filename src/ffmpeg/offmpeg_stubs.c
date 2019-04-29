#include "ffmpeg.h"

#include <libavutil/opt.h>

void print_error(const char *filename, int ret)
{
    av_log(NULL, AV_LOG_ERROR, "%s: %s\n", filename,
        av_err2str(ret));
}

static int decode_interrupt_cb(void *ctx)
{
    return 0;
}

const AVIOInterruptCB int_cb = { decode_interrupt_cb, NULL };


/***** Options *****/

void assert_empty_avoptions(AVDictionary *m)
{
    AVDictionaryEntry *t;
    if ((t = av_dict_get(m, "", NULL, AV_DICT_IGNORE_SUFFIX))) {
        av_log(NULL, AV_LOG_FATAL, "Option %s not found.\n", t->key);
        exit(1);
    }
}

static const AVOption * find_option(void *obj,
    const char* name, const char *key, int flags)
{
  const AVOption *o = av_opt_find(obj, key, NULL, 0,
      AV_OPT_SEARCH_CHILDREN | AV_OPT_SEARCH_FAKE_OBJ);
  if (!o) {
    av_log(NULL, AV_LOG_ERROR,
        "Unknown %s AVOption '%s'.\n", name, key);
    return NULL;
  } else if (!o->flags) {
    av_log(NULL, AV_LOG_ERROR,
        "Unmatched %s AVOption '%s'.\n", name, key);
    return NULL;
  } else if ((~o->flags & flags)) {
    av_log(NULL, AV_LOG_ERROR,
        "AVOption '%s' is not for %s.\n", key, name);
    return NULL;
  } else {
    av_log(NULL, AV_LOG_DEBUG,
        "Matched %s AVOption '%s'.\n", name, key);
    return o;
  }
}

/* XXX this doesn't handle private options (with
 * cc=codec->priv_class) */
void set_codec_option(AVDictionary **ptr_codec_opts, const char *key, const char *value, int is_output)
{
  const AVClass *cc = avcodec_get_class();
  const char *name = is_output ?
    "encoding" : "decoding";
  int flags = (is_output ?
      AV_OPT_FLAG_ENCODING_PARAM :
      AV_OPT_FLAG_DECODING_PARAM)
    | AV_OPT_FLAG_VIDEO_PARAM;
  const AVOption *o = find_option(&cc, name, key, flags);

  if (o) {
    av_dict_set(ptr_codec_opts, key, value, 0);
  }
}

void set_format_option(AVDictionary **ptr_format_opts, const char *key, const char *value, int is_output)
{
  const AVClass *fc = avformat_get_class();
  const char *name = is_output ?
    "muxing" : "demuxing";
  int flags = is_output ?
    AV_OPT_FLAG_ENCODING_PARAM :
    AV_OPT_FLAG_DECODING_PARAM;
  const AVOption *o = find_option(&fc, name, key, flags);

  if (o) {
    av_dict_set(ptr_format_opts, key, value, 0);
  }
}
