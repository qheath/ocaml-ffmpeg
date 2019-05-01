#include "ffmpeg.h"

#include "output_stubs.h"
#include "avfilter_stubs.h"

#include <libavfilter/buffersink.h>
#include <libavutil/opt.h>
#include <libavutil/pixdesc.h>


#include <libavutil/bprint.h>
#include <libavutil/intreadwrite.h>
#include <libavutil/time.h>


/***** Output file *****/

void free_output_file(OutputFile *output_file)
{
  if (output_file->ctx) {
    if (output_file->ctx->oformat) {
      avio_closep(&output_file->ctx->pb);
    }
    avformat_free_context(output_file->ctx);
  }

  av_freep(&output_file);
}

static void finalise_output_file(value v)
{
  OutputFile *output_file = OutputFile_val(v);
  free_output_file(output_file);
}

static struct custom_operations output_file_ops =
{
  "ocaml_output_file",
  finalise_output_file,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_output_file_value(OutputFile *output_file, value *pvalue)
{
  if (!output_file)
    Raise (EXN_FAILURE, "empty output_file");

  *pvalue = caml_alloc_custom(&output_file_ops, sizeof(output_file), 0, 1);
  OutputFile_val((*pvalue)) = output_file;
}

static OutputFile * alloc_output_file(value *pvalue)
{
  OutputFile *output_file;

  if (!(output_file = av_mallocz(sizeof(*output_file))))
    Raise (EXN_FAILURE, "failed to allocate output_file");

  alloc_output_file_value(output_file, pvalue);
  return output_file;
}

AVFormatContext * open_output_context(AVDictionary *protocol_options,
    const char *ofilename)
{
  int ret;
  AVFormatContext *ctx;

  ret = avformat_alloc_output_context2(&ctx,
      NULL, "mp4", ofilename);
  if (!ctx) {
    print_error(ofilename, ret);
    exit(1);
  }
  //ctx->oformat->flags =
  //  AVFMT_GLOBALHEADER | AVFMT_ALLOW_FLUSH | AVFMT_TS_NEGATIVE;
  // XXX
  ctx->interrupt_callback = int_cb;
  ctx->max_delay = (int)(0.7 * AV_TIME_BASE);
  //av_dict_set(&ctx->metadata, "creation_time", NULL, 0);

  /* open the output file with generic avio function,
   * get back unused protocol options */
  ret = avio_open2(&ctx->pb,
      ofilename,
      AVIO_FLAG_WRITE,
      &ctx->interrupt_callback,
      &protocol_options);
  if (ret< 0) {
    print_error(ofilename, ret);
    exit(1);
  }

  return ctx;
}

CAMLprim value make_output_file(value _protocol_options,
    value _ofilename)
{
  CAMLparam2(_protocol_options, _ofilename);
  CAMLlocal2(ans, pair);

  int i;
  OutputFile *output_file = alloc_output_file(&ans);
  int nb_protocol_options =
    Wosize_val(_protocol_options);
  AVDictionary *protocol_options = NULL;
  const char *ofilename = String_val(_ofilename);

  for (i=0; i<nb_protocol_options; i++) {
    pair = Field(_protocol_options, i);
    set_format_option(&protocol_options,
        String_val(Field(pair, 0)),
        String_val(Field(pair, 1)), 1);
  }

  output_file->ctx =
    open_output_context(protocol_options, ofilename);

  /* fail if there are format options left */
  assert_empty_avoptions(protocol_options);
  av_dict_free(&protocol_options);

  CAMLreturn(ans);
}

/* allocate output streams private data, write stream headers to file */
CAMLprim value open_muxer(value _muxer_options,
    value _output_file, value _output_streams)
{
  CAMLparam3(_muxer_options, _output_file, _output_streams);
  CAMLlocal1(pair);

  int ret, i, nb_muxer_options = Wosize_val(_muxer_options);
  AVDictionary *muxer_options = NULL;
  OutputFile *output_file = OutputFile_val(_output_file);
  int nb_output_streams = Wosize_val(_output_streams);

  for (i=0; i<nb_muxer_options; i++) {
    pair = Field(_muxer_options, i);
    set_format_option(&muxer_options,
        String_val(Field(pair, 0)),
        String_val(Field(pair, 1)), 1);
  }

  /* write the output file headers with generic avformat function,
   * get back unused muxer options */
  ret = avformat_write_header(output_file->ctx,
      &muxer_options);
  if (ret<0) {
    print_error("output_file->filename", ret);
    exit(1);
  }
  /* fail if there are format options left */
  assert_empty_avoptions(muxer_options);
  av_dict_free(&muxer_options);

  /* print detailed information about the output format */
  for (i=0; i<nb_output_streams; i++) {
    av_dump_format(output_file->ctx, i, output_file->ctx->url, 1);
  }

  CAMLreturn(Val_unit);
}


/***** Output stream *****/

CAMLprim value name_of_output_stream(value _output_stream)
{
  CAMLparam1(_output_stream);

  OutputStream *output_stream =
    OutputStream_val(_output_stream);

  CAMLreturn(caml_copy_string(output_stream->enc_ctx->codec ? output_stream->enc_ctx->codec->name : "?"));
}

static void finalise_output_stream(value v)
{
  OutputStream *output_stream = OutputStream_val(v);

  av_freep(&output_stream->enc_ctx->stats_in);
  avcodec_free_context(&output_stream->enc_ctx);
  av_freep(&output_stream);
}

static struct custom_operations output_stream_ops =
{
  "ocaml_output_stream",
  finalise_output_stream,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_output_stream_value(OutputStream *output_stream,
    value *pvalue)
{
  if (!output_stream)
    Raise (EXN_FAILURE, "empty output_stream");

  *pvalue = caml_alloc_custom(&output_stream_ops,
      sizeof(output_stream), 0, 1);
  OutputStream_val((*pvalue)) = output_stream;
}

static OutputStream * alloc_output_stream(value *pvalue)
{
  OutputStream *output_stream;

  if (!(output_stream = av_mallocz(sizeof(*output_stream))))
    Raise (EXN_FAILURE, "failed to allocate output_stream");

  alloc_output_stream_value(output_stream, pvalue);
  return output_stream;
}

static AVCodec *get_encoder(void)
{
  const AVCodecDescriptor *desc;
  AVCodec *codec;
  const char *codec_name = "libx264";

  if (!(codec = avcodec_find_encoder_by_name(codec_name)) &&
      (desc = avcodec_descriptor_get_by_name(codec_name))) {
    codec = avcodec_find_encoder(desc->id);
  }
  if (!codec)
    av_log(NULL, AV_LOG_FATAL, "Unknown encoder '%s'.\n", codec_name);

  return codec;
}

static void set_encoder(AVStream *st, AVCodec *codec)
{
  av_log(NULL, AV_LOG_VERBOSE,
      "Matched encoder '%s'.\n", codec->name);
  if (codec->type != st->codecpar->codec_type) {
    av_log(NULL, AV_LOG_FATAL,
        "Invalid encoder type.\n");
    exit(1);
  }

  st->codecpar->codec_id = codec->id;
}

/* Create an output stream, optionaly linked to an input stream. */
static void setup_output_stream(OutputFile *output_file,
    OutputStream *output_stream,
    const char *flags)
{
  AVFormatContext *oc = output_file->ctx;
  AVCodec *codec = get_encoder();
  AVStream *st;

  if (!(st = avformat_new_stream(oc, codec))) {
    av_log(NULL, AV_LOG_FATAL, "Could not alloc stream.\n");
    exit(1);
  }
  st->index                = oc->nb_streams - 1;
  st->sample_aspect_ratio  = (AVRational){1, 1};
  st->disposition          = AV_DISPOSITION_DEFAULT;
  st->codecpar->codec_type = AVMEDIA_TYPE_VIDEO;
  st->avg_frame_rate       = (AVRational){30000, 1001};

  set_encoder(st, codec);

  if (!(output_stream->enc_ctx =
        avcodec_alloc_context3(codec))) {
    av_log(NULL, AV_LOG_ERROR,
        "Error allocating the encoding context.\n");
    exit(1);
  }
  //if ((ret = avcodec_parameters_to_context(output_stream->enc_ctx, st->codecpar)) < 0) {
  //    av_log(NULL, AV_LOG_ERROR, "Error initializing the encoder context.\n");
  //    exit(1);
  //}
  output_stream->enc_ctx->rc_override_count      = 0;
  output_stream->enc_ctx->bits_per_raw_sample    = 0;
  output_stream->enc_ctx->chroma_sample_location = AVCHROMA_LOC_UNSPECIFIED;
  output_stream->enc_ctx->flags                 |= AV_CODEC_FLAG_GLOBAL_HEADER;
  output_stream->enc_ctx->sample_aspect_ratio    = st->sample_aspect_ratio;
  output_stream->enc_ctx->pix_fmt                = AV_PIX_FMT_YUV420P;
  output_stream->enc_ctx->time_base              = av_inv_q(st->avg_frame_rate);
  output_stream->enc_ctx->framerate              = st->avg_frame_rate;

  {
    uint8_t *encoder_string;
    int encoder_string_len;
    int codec_flags = output_stream->enc_ctx->flags;

    if (!av_dict_get(st->metadata, "encoder",  NULL, 0)) {
      const AVOption *o = av_opt_find(output_stream->enc_ctx,
          "flags", NULL, 0, 0);
      // XXX
      if (!o)
        exit(1);
      if (flags) {
        av_opt_eval_flags(output_stream->enc_ctx,
            o, flags, &codec_flags);
      }

      encoder_string_len = sizeof(LIBAVCODEC_IDENT) + strlen(output_stream->enc_ctx->codec->name) + 2;
      encoder_string     = av_mallocz(encoder_string_len);
      if (!encoder_string)
        exit(1);

      if (!(codec_flags & AV_CODEC_FLAG_BITEXACT))
        av_strlcpy(encoder_string, LIBAVCODEC_IDENT " ", encoder_string_len);
      else
        av_strlcpy(encoder_string, "Lavc ", encoder_string_len);
      av_strlcat(encoder_string, output_stream->enc_ctx->codec->name, encoder_string_len);
      av_dict_set(&st->metadata, "encoder",  encoder_string,
          AV_DICT_DONT_STRDUP_VAL | AV_DICT_DONT_OVERWRITE);
    }
  }

  output_stream->last_mux_dts = AV_NOPTS_VALUE;
}

/* set up the OutputStream */
CAMLprim value make_output_stream(value _output_file,
    value _flags)
{
  CAMLparam2(_output_file, _flags);
  CAMLlocal1(_stream);

  OutputFile *output_file =
    OutputFile_val(_output_file);
  OutputStream *output_stream;
  const char *flags =
    Is_block(_flags) ?  String_val(Field(_flags, 0)) : NULL;

  output_stream = alloc_output_stream(&_stream);
  setup_output_stream(output_file,
      output_stream, flags);

  CAMLreturn(_stream);
}

/* set up the Filter */
CAMLprim value init_output_filter(value _filter_graph,
    value _out_filter,
    value _output_file,
    value _stream_index,
    value _output_stream)
{
  CAMLparam5(_filter_graph, _out_filter,
      _output_file,
      _stream_index,
      _output_stream);
  CAMLlocal1(_output_filter);

  Filter *ofilter;
  int ret;
  char name[255];
  AVFilterGraph *filter_graph =
    FilterGraph_val(_filter_graph);
  AVFilterInOut *out =
    FilterInOut_val(_out_filter);
  AVFilterContext *out_filter_ctx =
    out->filter_ctx, *in_filter_ctx;
  int pad_idx = out->pad_idx;
  OutputFile *output_file =
    OutputFile_val(_output_file);
  int stream_index = Int_val(_stream_index);
  AVStream *st = output_file->ctx->streams[stream_index];
  OutputStream *output_stream =
    OutputStream_val(_output_stream);

  if (avfilter_pad_get_type(out_filter_ctx->output_pads,
        pad_idx) != AVMEDIA_TYPE_VIDEO) {
    av_log(NULL, AV_LOG_FATAL,
        "Only video filters are supported currently.\n");
    exit(1);
  }

  ofilter = alloc_filter(&_output_filter);

  ofilter->name =
    describe_filter_link(out_filter_ctx, pad_idx, 0);

  {
    snprintf(name, sizeof(name), "format :%d",
        st->index);

    if (!(in_filter_ctx =
          avfilter_graph_alloc_filter(filter_graph,
            avfilter_get_by_name("format"), name))) {
      av_log(NULL, AV_LOG_FATAL,
          "Error allocating format filter.\n");
    }

    if ((ret = avfilter_init_str(in_filter_ctx,
            av_get_pix_fmt_name(output_stream->enc_ctx->pix_fmt))) < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error setting up output filter: %s\n",
          av_err2str(ret));
      exit(1);
    }
    if ((ret = avfilter_link(out_filter_ctx, pad_idx, in_filter_ctx, 0)) < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error adding up output filter to graph: %s\n",
          av_err2str(ret));
      exit(1);
    }

    out_filter_ctx = in_filter_ctx;
    pad_idx = 0;
  }

  {
    snprintf(name, sizeof(name), "output stream :%d",
        st->index);

    if (!(in_filter_ctx =
          avfilter_graph_alloc_filter(filter_graph,
            avfilter_get_by_name("buffersink"), name))) {
      av_log(NULL, AV_LOG_FATAL,
          "Error allocating buffersink filter.\n");
    }

    if ((ret = avfilter_init_str(in_filter_ctx, NULL)) < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error setting up output filter: %s\n",
          av_err2str(ret));
      exit(1);
    }
    if ((ret = avfilter_link(out_filter_ctx, pad_idx, in_filter_ctx, 0)) < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error adding up output filter to graph: %s\n",
          av_err2str(ret));
      exit(1);
    }

    ofilter->filter_ctx = in_filter_ctx;
  }

  CAMLreturn(_output_filter);
}

static int init_output_stream(AVStream *st,
    AVCodecContext *enc_ctx)
{
  int ret = 0;

  //st->codec->codec = enc_ctx->codec;
  if ((ret = avcodec_parameters_from_context(st->codecpar,
          enc_ctx)) < 0) {
    av_log(NULL, AV_LOG_FATAL,
        "Error initializing the output stream codec context.\n");
    exit(1);
  }

  if (enc_ctx->nb_coded_side_data) {
    int i;

    for (i = 0; i < enc_ctx->nb_coded_side_data; i++) {
      const AVPacketSideData *sd_src =
        &enc_ctx->coded_side_data[i];
      uint8_t *dst_data;

      dst_data = av_stream_new_side_data(st,
          sd_src->type, sd_src->size);
      if (!dst_data)
        return AVERROR(ENOMEM);
      memcpy(dst_data, sd_src->data, sd_src->size);
    }
  }

  // copy timebase while removing common factors
  if (st->time_base.num <= 0 || st->time_base.den <= 0)
    st->time_base = av_add_q(enc_ctx->time_base,
        (AVRational){0, 1});
  else
    av_log(NULL, AV_LOG_ERROR,
        "st->timebase = %d/%d, should it be set?\n",
        st->time_base.num, st->time_base.den);

  // XXX know the uniduration and use it as a hint for the muxer
  //st->duration =
  //  av_rescale_q(st->duration, st->time_base, st->time_base);

  return ret;
}

CAMLprim value open_output_stream(value _output_file,
    value _stream_index,
    value _output_stream,
    value _codec_options,
    value _output_filter)
{
  CAMLparam5(_output_file, _stream_index,
    _output_stream, _codec_options, _output_filter);
  CAMLlocal1(pair);

  int i, ret;
  int nb_codec_options = Wosize_val(_codec_options);
  AVDictionary *codec_options = NULL;
  OutputFile *output_file =
    OutputFile_val(_output_file);
  int stream_index = Int_val(_stream_index);
  AVStream *st = output_file->ctx->streams[stream_index];
  OutputStream *output_stream =
    OutputStream_val(_output_stream);
  Filter *output_filter =
    Filter_val(_output_filter);

  output_stream->enc_ctx->width =
    av_buffersink_get_w(output_filter->filter_ctx);
  output_stream->enc_ctx->height =
    av_buffersink_get_h(output_filter->filter_ctx);
  output_stream->enc_ctx->pix_fmt =
    av_buffersink_get_format(output_filter->filter_ctx);
  if (!(output_stream->enc_ctx->time_base.num &&
        output_stream->enc_ctx->time_base.den))
    output_stream->enc_ctx->time_base =
      av_buffersink_get_time_base(output_filter->filter_ctx);

  for (i=0; i<nb_codec_options; i++) {
    pair = Field(_codec_options, i);
    set_codec_option(&codec_options,
        String_val(Field(pair, 0)),
        String_val(Field(pair, 1)), 1);
  }

  if ((ret = avcodec_open2(output_stream->enc_ctx,
          output_stream->enc_ctx->codec,
          &codec_options)) < 0) {
    exit(1);
  }
  assert_empty_avoptions(codec_options);
  av_dict_free(&codec_options);

  if ((ret = init_output_stream(st,
          output_stream->enc_ctx)) < 0) {
    av_log(NULL, AV_LOG_ERROR,
        "Error while opening encoder for output stream #%d - "
        "maybe incorrect parameters such as rate, width or height\n",
        i);
    exit(1);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value output_stream_eof(value _output_stream)
{
  CAMLparam1(_output_stream);
  CAMLlocal1(ans);

  OutputStream *output_stream = OutputStream_val(_output_stream);
  ans = Val_bool(output_stream->finished);

  CAMLreturn(ans);
}


/***** Print *****/

CAMLprim value dump_output_file(value _output_file)
{
  CAMLparam1(_output_file);

  OutputFile *output_file =
    OutputFile_val(_output_file);
  AVFormatContext *oc = output_file->ctx;

  if (!oc->nb_streams) {
    av_log(NULL, AV_LOG_ERROR,
        "Outpout does not contain any stream.\n");
    exit(1);
  }

  av_dump_format(oc, 0, oc->url, 1);

  CAMLreturn(Val_unit);
}

// XXX
static double psnr(double d)
{
    return -10.0 * log10(d);
}

static void print_stream_report(OutputFile *output_file,
    int stream_index,
    OutputStream *output_streams,
    int *pvid, AVBPrint *pbuf, AVBPrint *pbuf_script, int t, int is_last_report, int64_t *ppts, int nb_frames)
{
  AVStream *st = output_file->ctx->streams[stream_index];
  float q = -1;
  AVCodecContext *enc = output_streams->enc_ctx;

  if (enc->codec_type != AVMEDIA_TYPE_VIDEO)
    return;

  q = output_streams->quality / (float) FF_QP2LAMBDA;

  if (*pvid) {
    av_bprintf(pbuf, "q=%2.1f ", q);
    av_bprintf(pbuf_script, "stream_%d_q=%.1f\n",
        st->index, q);
  } else {
    float fps;

    fps = t > 1 ? nb_frames / t : 0;
    av_bprintf(pbuf, "frame=%5d fps=%3.*f q=%3.1f ",
        nb_frames, fps < 9.95, fps, q);
    av_bprintf(pbuf_script, "frame=%d\n", nb_frames);
    av_bprintf(pbuf_script, "fps=%.2f\n", fps);
    av_bprintf(pbuf_script, "stream_%d_q=%.1f\n",
        st->index, q);
    if (is_last_report)
      av_bprintf(pbuf, "L");

    if ((enc->flags & AV_CODEC_FLAG_PSNR) && (output_streams->pict_type != AV_PICTURE_TYPE_NONE || is_last_report)) {
      int j;
      double error, error_sum = 0;
      double scale, scale_sum = 0;
      double p;
      char type[3] = { 'Y','U','V' };
      av_bprintf(pbuf, "PSNR=");
      for (j = 0; j < 3; j++) {
        if (is_last_report) {
          error = enc->error[j];
          scale = enc->width * enc->height * 255.0 * 255.0 * nb_frames;
        } else {
          error = output_streams->error[j];
          scale = enc->width * enc->height * 255.0 * 255.0;
        }
        if (j)
          scale /= 4;
        error_sum += error;
        scale_sum += scale;
        p = psnr(error / scale);
        av_bprintf(pbuf, "%c:%2.2f ", type[j], p);
        av_bprintf(pbuf_script, "stream_%d_psnr_%c=%2.2f\n",
            st->index, type[j] | 32, p);
      }
      p = psnr(error_sum / scale_sum);
      av_bprintf(pbuf, "*:%2.2f ", psnr(error_sum / scale_sum));
      av_bprintf(pbuf_script, "stream_%d_psnr_all=%2.2f\n",
          st->index, p);
    }
    *pvid = 1;
  }
  /* compute min output value */
  if (av_stream_get_end_pts(st) != AV_NOPTS_VALUE)
    ppts = FFMAX(ppts, av_rescale_q(av_stream_get_end_pts(st),
          st->time_base, AV_TIME_BASE_Q));
}

CAMLprim value print_report(value _is_last_report,
    value _output_file, value _output_streams)
{
  CAMLparam3(_is_last_report,
      _output_file, _output_streams);
  CAMLlocal1(_output_stream);

  int is_last_report = Bool_val(_is_last_report);
  AVBPrint buf, buf_script;
  AVFormatContext *oc;
  int64_t total_size;
  int vid, i;
  double bitrate;
  double speed;
  int64_t pts = INT64_MIN + 1;
  static int64_t last_time = -1;
  static int64_t timer_start;
  int hours, mins, secs, us;
  const char *hours_sign;
  float t;
  int64_t cur_time = av_gettime_relative();
  OutputFile *output_file =
    OutputFile_val(_output_file);
  int nb_output_streams = Wosize_val(_output_streams);

  if (!is_last_report) {
    if (last_time == -1) {
      timer_start = last_time = cur_time;
      CAMLreturn(Val_unit);
    }
    if ((cur_time - last_time) < 500000)
      CAMLreturn(Val_unit);
    last_time = cur_time;
  }

  t = (cur_time-timer_start) / 1000000.0;

  oc = output_file->ctx;

  total_size = avio_size(oc->pb);
  if (total_size <= 0) // FIXME improve avio_size() so it works with non seekable output too
    total_size = avio_tell(oc->pb);

  vid = 0;
  av_bprint_init(&buf, 0, AV_BPRINT_SIZE_AUTOMATIC);
  av_bprint_init(&buf_script, 0, AV_BPRINT_SIZE_AUTOMATIC);
  for (i = 0; i < nb_output_streams; i++) {
    _output_stream = Field(_output_streams, i);
    print_stream_report(output_file, i,
        OutputStream_val(Field(_output_stream, 0)),
        &vid, &buf, &buf_script, t, is_last_report, &pts,
        Int_val(Field(_output_stream, 1)));
  }

  secs = FFABS(pts) / AV_TIME_BASE;
  us = FFABS(pts) % AV_TIME_BASE;
  mins = secs / 60;
  secs %= 60;
  hours = mins / 60;
  mins %= 60;
  hours_sign = (pts < 0) ? "-" : "";

  bitrate = pts && total_size >= 0 ? total_size * 8 / (pts / 1000.0) : -1;
  speed = t != 0.0 ? (double)pts / AV_TIME_BASE / t : -1;

  if (total_size < 0) av_bprintf(&buf, "size=N/A time=");
  else                av_bprintf(&buf, "size=%8.0fkB time=", total_size / 1024.0);
  if (pts == AV_NOPTS_VALUE) {
    av_bprintf(&buf, "N/A ");
  } else {
    av_bprintf(&buf, "%s%02d:%02d:%02d.%02d ",
        hours_sign, hours, mins, secs, (100 * us) / AV_TIME_BASE);
  }

  if (bitrate < 0) {
    av_bprintf(&buf, "bitrate=N/A");
    av_bprintf(&buf_script, "bitrate=N/A\n");
  }else{
    av_bprintf(&buf, "bitrate=%6.1fkbits/s", bitrate);
    av_bprintf(&buf_script, "bitrate=%6.1fkbits/s\n", bitrate);
  }

  if (total_size < 0) av_bprintf(&buf_script, "total_size=N/A\n");
  else                av_bprintf(&buf_script, "total_size=%"PRId64"\n", total_size);
  if (pts == AV_NOPTS_VALUE) {
    av_bprintf(&buf_script, "out_time_ms=N/A\n");
    av_bprintf(&buf_script, "out_time=N/A\n");
  } else {
    av_bprintf(&buf_script, "out_time_ms=%"PRId64"\n", pts);
    av_bprintf(&buf_script, "out_time=%s%02d:%02d:%02d.%06d\n",
        hours_sign, hours, mins, secs, us);
  }

  if (speed < 0) {
    av_bprintf(&buf, " speed=N/A");
    av_bprintf(&buf_script, "speed=N/A\n");
  } else {
    av_bprintf(&buf, " speed=%4.3gx", speed);
    av_bprintf(&buf_script, "speed=%4.3gx\n", speed);
  }

  {
    const char end = is_last_report ? '\n' : '\r';
    if (AV_LOG_INFO > av_log_get_level()) {
      fprintf(stderr, "%s    %c", buf.str, end);
    } else {
      av_log(NULL, AV_LOG_INFO, "%s    %c", buf.str, end);
    }

    fflush(stderr);
    av_bprint_finalize(&buf, NULL);

  }

  CAMLreturn(caml_copy_int64(total_size));
}


/***** Output *****/

/* send a frame to an encoder
 * should not be called after a receive_packet that returned 0
 * (receive_packet must be called until it returns AVERROR(EAGAIN) or
 * AVERROR_EOF (no need to call it anymore) */
static void send_frame(AVCodecContext *avctx, const AVFrame *frame)
{
    int ret;

    switch (ret = avcodec_send_frame(avctx, frame)) {
    case 0:
        break;
    default:
        av_log(NULL, AV_LOG_FATAL,
            "Unexpected error while sending frame: %s\n",
            av_err2str(ret));
        exit(1);
    }
}

/* 0: received a packet
 * AVERROR(EAGAIN): needs a frame to be send for another packet to be
 * received
 * AVERROR_EOF: encoder was already flushed */
CAMLprim value receive_packet(value _output_stream)
{
  CAMLparam1(_output_stream);
  CAMLlocal2(ans, _pkt);

  int ret;
  OutputStream *output_stream =
    OutputStream_val(_output_stream);
  AVCodecContext *avctx = output_stream->enc_ctx;
  AVPacket *pkt;

  pkt = alloc_packet_value(&_pkt);

  switch (ret = avcodec_receive_packet(avctx, pkt)) {
    case 0:
      ans = caml_alloc(1, 0);
      Store_field(ans, 0, _pkt);
      break;

    case AVERROR(EAGAIN):
      ans = caml_alloc(1, 1);
      Store_field(ans, 0, PVV_Again);
      break;

    case AVERROR_EOF:
      ans = caml_alloc(1, 1);
      Store_field(ans, 0, PVV_End_of_file);
      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error while receiving packet: %s.\n",
          av_err2str(ret));
      exit(1);
  }

  CAMLreturn(ans);
}

static inline av_const int mid_pred(int a, int b, int c)
{
    if(a>b){
        if(c>b){
            if(c>a) b=a;
            else    b=c;
        }
    }else{
        if(b>c){
            if(c>a) b=c;
            else    b=a;
        }
    }
    return b;
}

// XXX
CAMLprim value write_packet(value _output_file,
    value _stream_index,
    value _output_stream,
    value _pkt)
{
  CAMLparam4(_output_file, _stream_index,
      _output_stream, _pkt);

  OutputFile *output_file =
    OutputFile_val(_output_file);
  int stream_index = Int_val(_stream_index);
  AVStream *st = output_file->ctx->streams[stream_index];
  OutputStream *output_stream =
    OutputStream_val(_output_stream);
  AVPacket *pkt = Packet_val(_pkt);
  AVCodecContext *enc = output_stream->enc_ctx;
  AVFormatContext *s = output_file->ctx;
  int ret;
  int i;
  uint8_t *sd;

  if (pkt->pts == AV_NOPTS_VALUE && !(enc->codec->capabilities & AV_CODEC_CAP_DELAY)) {
    //pkt->pts = output_stream->last_pts;
    exit(1);
  }

  av_log(NULL, AV_LOG_ERROR,
      "output packet: pts=%ld ; dts=%ld\n",
      pkt->pts, pkt->dts);

  av_packet_rescale_ts(pkt, enc->time_base,
      st->time_base);

  sd = av_packet_get_side_data(pkt, AV_PKT_DATA_QUALITY_STATS,
      NULL);
  output_stream->quality = sd ? AV_RL32(sd) : -1;
  output_stream->pict_type = sd ? sd[4] : AV_PICTURE_TYPE_NONE;

  for (i = 0; i<FF_ARRAY_ELEMS(output_stream->error); i++) {
    if (sd && i < sd[5])
      output_stream->error[i] = AV_RL64(sd + 8 + 8*i);
    else
      output_stream->error[i] = -1;
  }

  if (st->avg_frame_rate.num) {
    if (pkt->duration > 0)
      av_log(NULL, AV_LOG_WARNING, "Overriding packet duration by frame rate, this should not happen\n");
    pkt->duration =
      av_rescale_q(1,
          av_inv_q(st->avg_frame_rate),
          st->time_base);
  }

  // XXX
  av_packet_rescale_ts(pkt, st->time_base, st->time_base);

  if (pkt->dts != AV_NOPTS_VALUE &&
      pkt->pts != AV_NOPTS_VALUE &&
      pkt->dts > pkt->pts) {
    av_log(s, AV_LOG_WARNING, "Invalid DTS: %"PRId64" PTS: %"PRId64" in output stream %d, replacing by guess\n",
        pkt->dts, pkt->pts, st->index);
    pkt->pts =
      pkt->dts = mid_pred(pkt->pts,
          pkt->dts,
          output_stream->last_mux_dts + 1);
  }
  if (pkt->dts != AV_NOPTS_VALUE && output_stream->last_mux_dts != AV_NOPTS_VALUE) {
    int64_t max = output_stream->last_mux_dts + 1;
    if (pkt->dts < max) {
      av_log(s, AV_LOG_WARNING,
          "Non-monotonous DTS in output stream "
          "%d; previous: %"PRId64", current: %"PRId64"; ",
          st->index, output_stream->last_mux_dts, pkt->dts);
      av_log(s, AV_LOG_WARNING,
          "changing to %"PRId64". This may result "
          "in incorrect timestamps in the output file.\n",
          max);
      if (pkt->pts >= pkt->dts)
        pkt->pts = FFMAX(pkt->pts, max);
      pkt->dts = max;
    }
  }
  output_stream->last_mux_dts = pkt->dts;

  pkt->stream_index = st->index;

  switch (ret = av_interleaved_write_frame(s, pkt)) {
    case 0:
      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error interleaving a frame: %s\n",
          av_err2str(ret));
      exit(1);
  }

  av_packet_unref(pkt);

  CAMLreturn(Val_unit);
}

CAMLprim value write_trailer(value _output_file)
{
  CAMLparam1(_output_file);

  int ret;
  OutputFile *output_file = OutputFile_val(_output_file);
  AVFormatContext *ctx = output_file->ctx;

  // XXX
  /* write the trailer if needed and close file */
  if ((ret = av_write_trailer(ctx)) < 0) {
    av_log(NULL, AV_LOG_ERROR,
        "Error writing trailer of %s: %s\n",
        ctx->url, av_err2str(ret));
    exit(1);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value send_frame_to_stream(value _output_stream,
    value _last_frame_opt)
{
  CAMLparam2(_output_stream, _last_frame_opt);
  CAMLlocal2(_last_frame, _pts);

  OutputStream *output_stream =
    OutputStream_val(_output_stream);

  if (Is_block(_last_frame_opt)) {
    _last_frame = Field(Field(_last_frame_opt, 0), 0);
    _pts = Field(Field(_last_frame_opt, 0), 1);
    AVFrame *last_frame = Frame_val(_last_frame);
    int64_t pts = Int64_val(_pts);
    int64_t last_frame_pts = last_frame->pts;

    last_frame->quality = output_stream->enc_ctx->global_quality;
    last_frame->pict_type = 0;

    last_frame->pts = pts;
    send_frame(output_stream->enc_ctx, last_frame);
    last_frame->pts = last_frame_pts;
  } else {
    av_log(NULL, AV_LOG_ERROR, "flush_output_stream\n");

    send_frame(output_stream->enc_ctx, NULL);

    output_stream->finished = 1;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value rescale_output_frame_pts(value _output_stream,
    value _output_filter,
    value _next_frame)
{
  CAMLparam3(_output_stream, _output_filter, _next_frame);
  CAMLlocal1(ans);

  OutputStream *output_stream =
    OutputStream_val(_output_stream);
  AVCodecContext *enc = output_stream->enc_ctx;
  Filter *output_filter = Filter_val(_output_filter);
  AVFilterContext *filter_ctx = output_filter->filter_ctx;
  AVFrame *next_frame = Frame_val(_next_frame);

  if (next_frame->pts != AV_NOPTS_VALUE) {
    int64_t pts =
      av_rescale_q(next_frame->pts,
          av_buffersink_get_time_base(filter_ctx),
          enc->time_base);

    /* XXX use this */
    int64_t duration =
      av_rescale_q(1,
          av_inv_q(av_buffersink_get_frame_rate(filter_ctx)),
          enc->time_base);

    av_log(NULL, AV_LOG_ERROR,
        "output frame: pts=%ld+%ld(%ld+%ld)\n",
        next_frame->pts, 1l,
        pts, duration);

    next_frame->pts = pts;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value setup_field_order(value _output_file,
    value _stream_index,
    value _last_frame)
{
  CAMLparam3(_output_file, _stream_index,
    _last_frame);

  OutputFile *output_file =
    OutputFile_val(_output_file);
  int stream_index = Int_val(_stream_index);
  AVStream *st = output_file->ctx->streams[stream_index];
  AVFrame *last_frame = Frame_val(_last_frame);

  if (last_frame->interlaced_frame)
    st->codecpar->field_order =
      last_frame->top_field_first ? AV_FIELD_TB:AV_FIELD_BT;
  else
    st->codecpar->field_order =
      AV_FIELD_PROGRESSIVE;

  CAMLreturn(Val_unit);
}

CAMLprim value frame_pts(value _frame)
{
  CAMLparam1(_frame);

  AVFrame *frame = Frame_val(_frame);

  CAMLreturn(caml_copy_int64(frame->pts));
}
