#include "ffmpeg.h"

#include "input_stubs.h"
#include "avfilter_stubs.h"

#include <libavfilter/buffersrc.h>
#include <libavutil/opt.h>
#include <libavutil/pixdesc.h>

#include <pthread.h>

#include <libavutil/time.h>


/***** Input file *****/

CAMLprim value input_file_name(value _input_file)
{
  CAMLparam1(_input_file);

  InputFile *input_file =
    InputFile_val(_input_file);

  CAMLreturn(caml_copy_string(input_file->ctx->url));
}

/* cause the input thread to stop */
static void free_input_thread(InputFile *input_file)
{
  AVPacket pkt;

  if (!input_file || !input_file->in_thread_queue)
    return;
  /* cause the input thread to stop and to set AVERROR_EOF to be
   * received */
  av_thread_message_queue_set_err_send(input_file->in_thread_queue, AVERROR_EOF);
  while (av_thread_message_queue_recv(input_file->in_thread_queue, &pkt, 0) >= 0)
    av_packet_unref(&pkt);

  pthread_join(input_file->thread, NULL);
  av_thread_message_queue_free(&input_file->in_thread_queue);
}

void free_input_file(InputFile *input_file)
{
  free_input_thread(input_file);
  avformat_close_input(&input_file->ctx);

  av_freep(&input_file);
}

static void finalise_input_file(value v)
{
  InputFile *input_file = InputFile_val(v);
  free_input_file(input_file);
}

static struct custom_operations input_file_ops =
{
  "ocaml_input_file",
  finalise_input_file,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_input_file_value(InputFile *input_file, value *pvalue)
{
  if (!input_file)
    Raise (EXN_FAILURE, "empty input_file");

  *pvalue = caml_alloc_custom(&input_file_ops, sizeof(input_file), 0, 1);
  InputFile_val((*pvalue)) = input_file;
}

static InputFile * alloc_input_file(value *pvalue)
{
  InputFile *input_file;

  if (!(input_file = av_mallocz(sizeof(*input_file))))
    Raise (EXN_FAILURE, "failed to allocate input_file");

  alloc_input_file_value(input_file, pvalue);
  return input_file;
}

AVFormatContext * setup_input_context(AVDictionary *format_options,
    const char *ifilename)
{
  int ret;
  int i;
  AVFormatContext *ctx;

  if (!(ctx = avformat_alloc_context())) {
    print_error(ifilename, AVERROR(ENOMEM));
    exit(1);
  }
  ctx->video_codec_id     = AV_CODEC_ID_NONE;
  ctx->flags             |= AVFMT_FLAG_NONBLOCK;
  ctx->interrupt_callback = int_cb;

  /* open the input file with generic avformat function,
   * get back unused format options */
  ret = avformat_open_input(&ctx,
      ifilename,
      NULL,
      &format_options);
  if (ret<0) {
    print_error(ifilename, ret);
    exit(1);
  }

  /* If not enough info to get the stream parameters, we
   * decode the first frames to get it. (used in mpeg
   * case for example) */
  avformat_find_stream_info(ctx, NULL);

  for (i=0; i<ctx->nb_streams; i++) {
    /* print detailed information about the input format */
    av_dump_format(ctx, i, ctx->url, 0);
  }

  return ctx;
}

CAMLprim value make_input_file(value _format_options,
    value _ifilename)
{
  CAMLparam2(_format_options, _ifilename);
  CAMLlocal3(ans, pair, _input_file);

  int i;
  InputFile *input_file = alloc_input_file(&_input_file);
  int nb_format_options =
    Wosize_val(_format_options);
  AVDictionary *format_options = NULL;
  const char *ifilename = String_val(_ifilename);

  for (i=0; i<nb_format_options; i++) {
    pair = Field(_format_options, i);
    set_format_option(&format_options,
        String_val(Field(pair, 0)),
        String_val(Field(pair, 1)), 0);
  }

  input_file->ctx =
    setup_input_context(format_options, ifilename);

  /* fail if there are format options left */
  assert_empty_avoptions(format_options);
  av_dict_free(&format_options);

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, _input_file);
  Store_field(ans, 1, Val_int(input_file->ctx->nb_streams));

  CAMLreturn(ans);
}

/* read packets and send them in the thread queue, until an error
 * (including AVERROR_EOF but not AVERROR(EAGAIN)) is raised and set to
 * be received on the thread queue */
static void *input_thread(void *arg)
{
    InputFile *input_file = arg;
    int ret;
    AVPacket pkt;

    while (1) {
        /* wait for a packet to be read, or an error to be raised */
        while ((ret = av_read_frame(input_file->ctx, &pkt)) == AVERROR(EAGAIN)) {
            av_usleep(10000);
        }
        /* set any error (but AVERROR(EAGAIN)) to be received */
        if (ret < 0) {
            av_thread_message_queue_set_err_recv(input_file->in_thread_queue, ret);
            break;
        }

        /* wait for a packet to be sent, or an error to be raised */
        if (input_file->non_blocking) {
            while ((ret = av_thread_message_queue_send(input_file->in_thread_queue, &pkt, AV_THREAD_MESSAGE_NONBLOCK)) == AVERROR(EAGAIN)) {
                av_usleep(10000);
            }
        } else {
            ret = av_thread_message_queue_send(input_file->in_thread_queue, &pkt, 0);
        }
        /* set any error (but AVERROR(EAGAIN)) to be received */
        if (ret < 0) {
            if (ret != AVERROR_EOF)
                av_log(input_file->ctx, AV_LOG_ERROR,
                       "Unable to send packet to main thread: %s\n",
                       av_err2str(ret));
            av_packet_unref(&pkt);
            av_thread_message_queue_set_err_recv(input_file->in_thread_queue, ret);
            break;
        }
    }

    return NULL;
}

/* allocate a new message queue and create a thread */
CAMLprim value init_input_thread(value _input_file)
{
  CAMLparam1(_input_file);

  int ret;
  InputFile *input_file = InputFile_val(_input_file);

  if (!input_file->ctx->pb->seekable)
    input_file->non_blocking = 1;
  if ((ret = av_thread_message_queue_alloc(&input_file->in_thread_queue, 8, sizeof(AVPacket))) < 0) {
    av_log(NULL, AV_LOG_FATAL,
        "Unexpected error while allocate a new message queue: %s\n",
        av_err2str(ret));
    exit(1);
  }

  switch (ret = pthread_create(&input_file->thread,
        NULL, input_thread, input_file)) {
    case 0:
      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error while creating thread: %s. Try to increase `ulimit -v` or decrease `ulimit -s`.\n",
          strerror(ret));
      //av_thread_message_queue_free(&input_file->in_thread_queue);
      exit(1);
  }

  CAMLreturn(Val_unit);
}


/***** Input stream *****/

CAMLprim value media_type_of_input_stream(value _input_stream)
{
  CAMLparam1(_input_stream);

  InputStream *input_stream =
    InputStream_val(_input_stream);

  CAMLreturn(caml_copy_string(av_get_media_type_string(input_stream->dec_ctx->codec_type)));
}

CAMLprim value name_of_input_stream(value _input_stream)
{
  CAMLparam1(_input_stream);

  InputStream *input_stream =
    InputStream_val(_input_stream);

  CAMLreturn(caml_copy_string(input_stream->dec_ctx->codec ? input_stream->dec_ctx->codec->name : "?"));
}

static void finalise_input_stream(value v)
{
  InputStream *input_stream = InputStream_val(v);

  /* close decoders */
  avcodec_close(input_stream->dec_ctx);
  avcodec_free_context(&input_stream->dec_ctx);

  av_freep(&input_stream);
}

static struct custom_operations input_stream_ops =
{
  "ocaml_input_stream",
  finalise_input_stream,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_input_stream_value(InputStream *input_stream,
    value *pvalue)
{
  if (!input_stream)
    Raise (EXN_FAILURE, "empty input_stream");

  *pvalue = caml_alloc_custom(&input_stream_ops,
      sizeof(input_stream), 0, 1);
  InputStream_val((*pvalue)) = input_stream;
}

static InputStream * alloc_input_stream(value *pvalue)
{
  InputStream *input_stream;

  if (!(input_stream = av_mallocz(sizeof(*input_stream))))
    Raise (EXN_FAILURE, "failed to allocate input_stream");

  alloc_input_stream_value(input_stream, pvalue);
  return input_stream;
}

static AVCodecContext * alloc_codec_context(AVCodecParameters *codecpar)
{
  int ret;
  AVCodec *codec;
  AVCodecContext *dec_ctx;

  if (!(codec = avcodec_find_decoder(codecpar->codec_id))) {
    av_log(NULL, AV_LOG_FATAL,
        "Undentified decoder %d.\n", codecpar->codec_id);
    exit(1);
  }
  if (codec->type != codecpar->codec_type) {
    av_log(NULL, AV_LOG_FATAL,
        "Invalid decoder type '%s'.\n",
        codec->name);
    exit(1);
  }
  if (codecpar->codec_type != AVMEDIA_TYPE_VIDEO) {
    av_log(NULL, AV_LOG_FATAL,
        "Only video input streams supported.\n");
    exit(1);
  }
  av_log(NULL, AV_LOG_VERBOSE,
      "Matched decoder '%s'.\n", codec->name);

  if (!(dec_ctx = avcodec_alloc_context3(codec))) {
    av_log(NULL, AV_LOG_ERROR,
        "Error allocating the decoder context.\n");
    exit(1);
  }
  if ((ret = avcodec_parameters_to_context(dec_ctx,
          codecpar)) < 0) {
    av_log(NULL, AV_LOG_ERROR,
        "Error initializing the decoder context.\n");
    exit(1);
  }

  return dec_ctx;
}

static enum AVPixelFormat get_format(AVCodecContext *s, const enum AVPixelFormat *pix_fmts)
{
    const enum AVPixelFormat *p;

    for (p = pix_fmts; *p != AV_PIX_FMT_NONE; p++) {
        const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(*p);

        if (!(desc->flags & AV_PIX_FMT_FLAG_HWACCEL))
            break;
    }

    return *p;
}

static AVCodecContext * setup_codec_context(AVDictionary *codec_options,
    AVStream *st)
{
  int ret;
  AVCodecContext *dec_ctx =
    alloc_codec_context(st->codecpar);

  dec_ctx->framerate             = st->avg_frame_rate;
  dec_ctx->get_format            = get_format;
  dec_ctx->get_buffer2           = avcodec_default_get_buffer2;
  dec_ctx->thread_safe_callbacks = 1;
  dec_ctx->pkt_timebase          = st->time_base;

  if ((ret = avcodec_open2(dec_ctx, dec_ctx->codec,
          &codec_options)) < 0) {
    av_log(NULL, AV_LOG_ERROR,
        "Error opening the decoder: %s\n",
        av_err2str(ret));
    exit(1);
  }
  assert_empty_avoptions(codec_options);
  av_dict_free(&codec_options);

  return dec_ctx;
}

CAMLprim value make_input_stream(value _input_file,
    value _index,
    value _codec_options)
{
  CAMLparam3(_input_file, _index, _codec_options);
  CAMLlocal2(_input_stream, _codec_option);

  int i;
  InputFile *input_file = InputFile_val(_input_file);
  int index = Int_val(_index);
  AVStream *st = input_file->ctx->streams[index];
  int nb_codec_options = Wosize_val(_codec_options);
  AVDictionary *codec_options = NULL;

  InputStream *input_stream =
    alloc_input_stream(&_input_stream);

  st = input_file->ctx->streams[index];
  /* enable all frames */
  st->discard = AVDISCARD_NONE;

  for (i=0; i<nb_codec_options; i++) {
    _codec_option = Field(_codec_options, i);
    set_codec_option(&codec_options,
        String_val(Field(_codec_option, 0)),
        String_val(Field(_codec_option, 1)), 0);
  }

  input_stream->dec_ctx =
    setup_codec_context(codec_options, st);

  /* express the decoding offset in terms of AV_TIME_BASE_Q,
   * in case the first packets don't have valid dts fields */
  input_stream->next_dts =
    st->avg_frame_rate.num ?
    - av_rescale_q(input_stream->dec_ctx->has_b_frames, av_inv_q(st->avg_frame_rate), AV_TIME_BASE_Q) : 0l;

  input_stream->next_pts        = 0l;

  CAMLreturn(_input_stream);
}

/* set up the Filter */
CAMLprim value init_input_filter(value _input_file,
    value _index,
    value _filter_graph,
    value _in_filter)
{
  CAMLparam4(_input_file, _index,
      _filter_graph, _in_filter);
  CAMLlocal1(_input_filter);

  int ret;
  char name[255];
  Filter *ifilter;
  AVBufferSrcParameters *par;
  InputFile *input_file = InputFile_val(_input_file);
  int index = Int_val(_index);
  AVStream *st = input_file->ctx->streams[index];
  AVFilterGraph *filter_graph =
    FilterGraph_val(_filter_graph);
  AVFilterInOut *in =
    FilterInOut_val(_in_filter);
  AVFilterContext *in_filter_ctx =
    in->filter_ctx, *out_filter_ctx;
  int pad_idx = in->pad_idx;

  ifilter = alloc_filter(&_input_filter);

  ifilter->name =
    describe_filter_link(in_filter_ctx, pad_idx, 1);

  {
    snprintf(name, sizeof(name), "input stream %d:%d",
        0, st->index);

    if (!(out_filter_ctx =
          avfilter_graph_alloc_filter(filter_graph,
            avfilter_get_by_name("buffer"), name))) {
      av_log(NULL, AV_LOG_FATAL,
          "Error allocating buffer filter.\n");
      exit(1);
    }

    if (!(par = av_buffersrc_parameters_alloc())) {
      av_log(NULL, AV_LOG_FATAL,
          "Error allocating filter parameters.\n");
      exit(1);
    }
    memset(par, 0, sizeof(*par));

    par->width               = st->codecpar->width;
    par->height              = st->codecpar->height;
    par->format              = st->codecpar->format;
    par->time_base           = st->time_base;
    par->sample_aspect_ratio = st->codecpar->sample_aspect_ratio;
    par->frame_rate          = av_guess_frame_rate(NULL, st, NULL);

    ret =
      av_buffersrc_parameters_set(out_filter_ctx, par);
    av_freep(&par);
    if (ret < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error setting input filter parameters: %s\n",
          av_err2str(ret));
      exit(1);
    }
    if ((ret = avfilter_init_str(out_filter_ctx, NULL)) < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error setting up input filter: %s\n",
          av_err2str(ret));
      exit(1);
    }
    if ((ret = avfilter_link(out_filter_ctx, 0, in_filter_ctx, pad_idx)) < 0) {
      av_log(NULL, AV_LOG_FATAL,
          "Error adding up input filter to graph: %s\n",
          av_err2str(ret));
      exit(1);
    }

    ifilter->filter_ctx = out_filter_ctx;
  }

  CAMLreturn(_input_filter);
}


/***** Print *****/


/***** Packet *****/

CAMLprim value make_packet_from_file(value _input_file)
{
  CAMLparam1(_input_file);
  CAMLlocal2(ans, _pkt);

  int ret;
  InputFile *input_file = InputFile_val(_input_file);
  AVPacket *pkt = alloc_packet_value(&_pkt);

  switch (ret = av_thread_message_queue_recv(input_file->in_thread_queue, pkt,
        input_file->non_blocking ?
        AV_THREAD_MESSAGE_NONBLOCK : 0)) {
    case 0:
      ans = caml_alloc(1, 0);
      Store_field(ans, 0, _pkt);

      break;

    case AVERROR_EOF:
      ans = Val_int(0);

      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error while receiving frame from %s: %s\n",
          input_file->ctx->url, av_err2str(ret));
      av_freep(pkt);
      exit(1);
  }

  CAMLreturn(ans);
}


/***** Input *****/

/* send a packet to a decoder */
CAMLprim value send_packet(value _input_stream,
    value _pkt)
{
  CAMLparam2(_input_stream, _pkt);
  CAMLlocal1(ans);

  int ret;
  InputStream *input_stream = InputStream_val(_input_stream);
  AVPacket *pkt =
    Is_block(_pkt) ? Packet_val(Field(_pkt, 0)) : NULL;

  switch (ret = avcodec_send_packet(input_stream->dec_ctx,
        pkt)) {
    case 0:
      ans = PVV_Ok;
      break;

    case AVERROR(EAGAIN):
      ans = PVV_Again;
      break;

    case AVERROR_EOF:
      ans = PVV_End_of_file;
      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error while sending packet: %s.\n",
          av_err2str(ret));
      exit(1);
  }

  CAMLreturn(ans);
}

/* 0: received a frame
 * AVERROR(EAGAIN): needs a packet to be sent for another frame to be received
 * AVERROR_EOF: decoder was already flushed */
CAMLprim value receive_frame(value _input_stream)
{
  CAMLparam1(_input_stream);
  CAMLlocal2(ans, _frame);

  int ret;
  AVFrame *frame;
  InputStream *input_stream =
    InputStream_val(_input_stream);

  frame = alloc_frame_value(&_frame);

  switch (ret = avcodec_receive_frame(input_stream->dec_ctx,
        frame)) {
    case 0:
      ans = caml_alloc(1, 0);
      Store_field(ans, 0, _frame);
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
          "Unexpected error while receiving frame: %s.\n",
          av_err2str(ret));
      exit(1);
  }

  CAMLreturn(ans);
}

/* add a frame to the buffer source */
static void send_frame_to_filter(Filter *ifilter, AVFrame *frame)
{
    int ret;

    switch (ret = av_buffersrc_add_frame_flags(ifilter->filter_ctx, frame, AV_BUFFERSRC_FLAG_PUSH)) {
    case 0:
        break;

    default:
        av_log(NULL, AV_LOG_FATAL,
            "Unexpected error while injecting frame into filter network: %s\n",
            av_err2str(ret));
        exit(1);
    }
}

/* send a decoded_frame in the input filter
 * attached to the stream */
static void send_frame_to_filters(InputStream *ist,
    value _input_filter, AVFrame *frame)
{
  Filter *input_filter;
  input_filter = Filter_val(_input_filter);
  send_frame_to_filter(input_filter, frame);
}

CAMLprim value filter_frame(value _input_file,
    value _index,
    value _input_stream,
    value _input_filter, value _frame)
{
  CAMLparam5 (_input_file, _index,
    _input_stream, _input_filter, _frame);

  InputFile *input_file = InputFile_val(_input_file);
  int index = Int_val(_index);
  AVStream *st = input_file->ctx->streams[index];
  InputStream *ist =
    InputStream_val(_input_stream);
  AVFrame *frame = Frame_val(_frame);

  frame->pts =
    frame->best_effort_timestamp;
  // refine next_pts with what the decoder came up with
  if ((frame->pts) != AV_NOPTS_VALUE) {
    ist->next_pts =
      av_rescale_q(frame->pts,
          st->time_base, AV_TIME_BASE_Q);
  } else {
    frame->pts =
      av_rescale_q(ist->next_pts,
          AV_TIME_BASE_Q, st->time_base);
  }

  ist->next_pts += (frame->pkt_duration > 0) ?
    // use the explicit duration
    av_rescale_q(frame->pkt_duration,
        st->time_base, AV_TIME_BASE_Q) :
    // use an aproximation based on the average framerate
    av_rescale_q(1,
        av_inv_q(ist->dec_ctx->framerate),
        AV_TIME_BASE_Q);

  av_log(NULL, AV_LOG_VERBOSE,
      "input frame: pts=%ld(%ld)\n",
      frame->pts, ist->next_pts);

  if (st->sample_aspect_ratio.num)
    frame->sample_aspect_ratio =
      st->sample_aspect_ratio;

  send_frame_to_filters(ist,
      _input_filter,
      frame);

  av_frame_unref(frame);

  CAMLreturn(Val_unit);
}

/* flush the buffer source filter */
static void flush_input_filter(Filter *ifilter, int64_t pts)
{
    int ret;

    switch (ret = av_buffersrc_close(ifilter->filter_ctx, pts, AV_BUFFERSRC_FLAG_PUSH)) {
    case 0:
        break;

    default:
        av_log(NULL, AV_LOG_FATAL,
            "Unexpected error while marking filters as finished %s\n",
            av_err2str(ret));
        exit(1);
    }
}

CAMLprim value get_input_stream_index_from_filter(value _input_file,
    value _in_filter)
{
  CAMLparam2(_input_file, _in_filter);

  int i;
  InputFile *input_file;
  AVFilterInOut *in_filter =
    FilterInOut_val(_in_filter);
  AVFilterContext *in_filter_ctx =
    in_filter->filter_ctx;
  char *in_filter_name = in_filter->name;
  int in_filter_pad_idx = in_filter->pad_idx;
  int input_stream_index;
  char *p;
  AVStream *st;

  // XXX pick the right input_file
  //int file_idx;
  //if ((file_idx = strtol(in_filter_name, &p, 0)) < 0 ||
  //    file_idx >= nb_input_files) {
  //    av_log(NULL, AV_LOG_FATAL,
  //        "Invalid file index %d in filtergraph description.\n",
  //        file_idx);
  //    exit(1);
  //}
  //input_file = input_files[file_idx];
  strtol(in_filter_name, &p, 0);
  input_file = InputFile_val(_input_file);

  if (avfilter_pad_get_type(in_filter_ctx->input_pads, in_filter_pad_idx) != AVMEDIA_TYPE_VIDEO) {
    av_log(NULL, AV_LOG_FATAL, "Only video filters supported.\n");
    exit(1);
  }
  if (!in_filter_name) {
    av_log(NULL, AV_LOG_FATAL, "Only named inputs supported.\n");
    exit(1);
  }

  for (i = 0, st = NULL; i < input_file->ctx->nb_streams; i++) {
    if (avformat_match_stream_specifier(input_file->ctx, input_file->ctx->streams[i], *p == ':' ? p + 1 : p) == 1) {
      st = input_file->ctx->streams[i];
      break;
    }
  }
  if (!st) {
    av_log(NULL, AV_LOG_FATAL,
        "Stream specifier '%s' in filtergraph description matches no streams.\n",
        p);
    exit(1);
  }
  input_stream_index = st->index;

  CAMLreturn(Val_int(input_stream_index));
}

CAMLprim value rescale_input_packet_dts(value _input_file,
    value _index,
    value _input_stream,
    value _pkt)
{
  CAMLparam4(_input_file, _index,
    _input_stream, _pkt);
  CAMLlocal1(ans);

  InputFile *input_file = InputFile_val(_input_file);
  int index = Int_val(_index);
  AVStream *st = input_file->ctx->streams[index];
  InputStream *ist =
    InputStream_val(_input_stream);
  AVPacket *pkt = Packet_val(_pkt);

  // refine next_dts with what the packet contains
  if (pkt->dts == AV_NOPTS_VALUE) {
    pkt->dts =
      av_rescale_q(ist->next_dts,
          AV_TIME_BASE_Q, st->time_base);
  } else {
    ist->next_dts =
      av_rescale_q(pkt->dts,
          st->time_base, AV_TIME_BASE_Q);
  }

  ist->next_dts += pkt->duration ?
    // use the explicit duration
    av_rescale_q(pkt->duration, st->time_base, AV_TIME_BASE_Q) :
    // use an aproximation based on the average framerate
    av_rescale_q(1, av_inv_q(ist->dec_ctx->framerate), AV_TIME_BASE_Q);

  CAMLreturn(Val_unit);
}

/* flush the decoder and the filter inputs */
CAMLprim value flush_input_stream(value _input_file,
    value _index,
    value _input_stream,
    value _input_filter)
{
  CAMLparam4(_input_file, _index,
      _input_stream, _input_filter);

  int64_t next_pts;
  InputFile *input_file = InputFile_val(_input_file);
  int index = Int_val(_index);
  AVStream *st = input_file->ctx->streams[index];
  InputStream *ist = InputStream_val(_input_stream);
  Filter *input_filter = Filter_val(_input_filter);

  av_log(NULL, AV_LOG_VERBOSE,
    "flush_input_stream\n");

  /* flush all the filter inputs attached to the stream */
  next_pts =
    av_rescale_q(ist->next_pts,
        AV_TIME_BASE_Q, st->time_base);
  flush_input_filter(input_filter, next_pts);

  CAMLreturn(Val_unit);
}
