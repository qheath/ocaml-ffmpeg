#include "ffmpeg.h"

#include "avfilter_stubs.h"

#include <libavfilter/avfilter.h>
#include <libavfilter/buffersrc.h>
#include <libavfilter/buffersink.h>


/***** FilterInOut *****/

CAMLprim value filter_in_out_name(value _filter_in_out)
{
  CAMLparam1(_filter_in_out);
  CAMLlocal1(ans);

  AVFilterInOut *filter_in_out =
    FilterInOut_val(_filter_in_out);

  ans = caml_copy_string(filter_in_out->name);

  CAMLreturn(ans);
}

static void finalise_filter_in_out(value v)
{
  AVFilterInOut *filter_in_out = FilterInOut_val(v);

  av_freep(&filter_in_out->name);
  avfilter_inout_free(&filter_in_out);
}

static struct custom_operations filter_in_out_ops =
{
  "ocaml_filter_in_out",
  finalise_filter_in_out,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_filter_in_out_value(AVFilterInOut *filter_in_out,
    value *pvalue)
{
  if (!filter_in_out)
    Raise (EXN_FAILURE, "empty filter_in_out");

  *pvalue = caml_alloc_custom(&filter_in_out_ops,
      sizeof(filter_in_out), 0, 1);
  FilterInOut_val((*pvalue)) = filter_in_out;
}

static AVFilterInOut * alloc_filter_in_out(value *pvalue)
{
  AVFilterInOut *filter_in_out;

  if (!(filter_in_out = avfilter_inout_alloc()))
    Raise (EXN_FAILURE, "failed to allocate filter_in_out");

  alloc_filter_in_out_value(filter_in_out, pvalue);
  return filter_in_out;
}

static void copy_filter_in_out(AVFilterInOut *cur,
    value *pvalue)
{
  AVFilterInOut *filter_in_out =
    alloc_filter_in_out(pvalue);

  filter_in_out->name = av_strdup(cur->name);
  filter_in_out->filter_ctx = cur->filter_ctx;
  filter_in_out->pad_idx = cur->pad_idx;
  filter_in_out->next = NULL;
}

static void convert_filters_in_out(AVFilterInOut **pfirst,
    value *pfilters)
{
  CAMLparam0();
  CAMLlocal1(filter);

  AVFilterInOut *cur;
  int i;

  for (cur = *pfirst, i = 0; cur; cur = cur->next, i++) {
  }

  *pfilters = caml_alloc_tuple(i);

  for (cur = *pfirst, i = 0; cur; cur = cur->next, i++) {
    copy_filter_in_out(cur, &filter);
    Store_field(*pfilters, i, filter);
  }

  avfilter_inout_free(pfirst);

  CAMLreturn0;
}


/***** Filter (AVFilterContext) *****/

char *describe_filter_link(AVFilterContext *ctx, int pad_idx, int in)
{
    AVFilterPad *pads = in ? ctx->input_pads  : ctx->output_pads;
    int       nb_pads = in ? ctx->nb_inputs   : ctx->nb_outputs;
    AVIOContext *pb;
    uint8_t *res = NULL;

    if (avio_open_dyn_buf(&pb) < 0)
        exit(1);

    avio_printf(pb, "%s", ctx->filter->name);
    if (nb_pads > 1)
        avio_printf(pb, ":%s", avfilter_pad_get_name(pads, pad_idx));
    avio_w8(pb, 0);
    avio_close_dyn_buf(pb, &res);
    return res;
}

static void finalise_filter(value v)
{
  Filter *filter = Filter_val(v);

  av_freep(&filter->name);
  av_freep(&filter);
}

static struct custom_operations filter_ops =
{
  "ocaml_filter",
  finalise_filter,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_filter_value(Filter *filter,
    value *pvalue)
{
  if (!filter)
    Raise (EXN_FAILURE, "empty filter");

  *pvalue = caml_alloc_custom(&filter_ops,
      sizeof(filter), 0, 1);
  Filter_val((*pvalue)) = filter;
}

/*static*/ Filter * alloc_filter(value *pvalue)
{
  Filter *filter;

  if (!(filter = av_mallocz(sizeof(*filter))))
    Raise (EXN_FAILURE, "failed to allocate filter");

  alloc_filter_value(filter, pvalue);
  return filter;
}

CAMLprim value name_of_filter(value _filter)
{
  CAMLparam1(_filter);

  Filter *filter = Filter_val(_filter);

  CAMLreturn(caml_copy_string(filter->name));
}

CAMLprim value get_nb_failed_requests(value _input_filter)
{
  CAMLparam1(_input_filter);
  CAMLlocal1(ans);

  Filter *input_filter = Filter_val(_input_filter);
  int nb =
    av_buffersrc_get_nb_failed_requests(input_filter->filter_ctx);

  ans = Val_int(nb);

  CAMLreturn(ans);
}

CAMLprim value buffersink_get_frame(value _output_filter)
{
  CAMLparam1(_output_filter);
  CAMLlocal2(ans, _next_frame);

  int ret;
  Filter *output_filter = Filter_val(_output_filter);
  AVFrame *next_frame =
    alloc_frame_value(&_next_frame);

  ret = av_buffersink_get_frame_flags(output_filter->filter_ctx,
      next_frame, AV_BUFFERSINK_FLAG_NO_REQUEST);

  switch (ret) {
    case 0:
      ans = caml_alloc(1, 0);
      Store_field(ans, 0, _next_frame);
      break;

    case AVERROR_EOF:
      ans = caml_alloc(1, 1);
      Store_field(ans, 0, PVV_End_of_file);
      break;

    case AVERROR(EAGAIN):
      ans = caml_alloc(1, 1);
      Store_field(ans, 0, PVV_Again);
      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error getting a frame from a buffer sink: %s\n",
          av_err2str(ret));
      exit(1);
  }

  CAMLreturn(ans);
}


/***** FilterGraph *****/

static void finalise_filter_graph(value v)
{
  AVFilterGraph *filter_graph = FilterGraph_val(v);

  avfilter_graph_free(&filter_graph);
}

static struct custom_operations filter_graph_ops =
{
  "ocaml_filter_graph",
  finalise_filter_graph,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};

static void alloc_filter_graph_value(AVFilterGraph *filter_graph,
    value *pvalue)
{
  if (!filter_graph)
    Raise (EXN_FAILURE, "empty filter_graph");

  *pvalue = caml_alloc_custom(&filter_graph_ops,
      sizeof(filter_graph), 0, 1);
  FilterGraph_val((*pvalue)) = filter_graph;
}

static AVFilterGraph * alloc_filter_graph(value *pvalue)
{
  AVFilterGraph *filter_graph;

  if (!(filter_graph = avfilter_graph_alloc()))
    Raise (EXN_FAILURE, "failed to allocate filter graph");

  alloc_filter_graph_value(filter_graph, pvalue);
  return filter_graph;
}

/* create the complex filtergraphs,
 * initialise output streams */
CAMLprim value make_filter_graph(value _filter_graph_desc)
{
  CAMLparam1(_filter_graph_desc);
  CAMLlocal1(_filter_graph);
  CAMLlocal2(in_filters, out_filters);
  CAMLlocal1(ans);

  int ret;
  AVFilterGraph *filter_graph =
    alloc_filter_graph(&_filter_graph);
  AVFilterInOut *inputs, *outputs;

  filter_graph->nb_threads = 4;

  if ((ret = avfilter_graph_parse2(filter_graph,
          String_val(_filter_graph_desc),
          &inputs, &outputs)) < 0) {
    av_log(NULL, AV_LOG_FATAL,
        "Error initializing filter graph: %s\n",
        av_err2str(ret));
    exit(1);
  }

  convert_filters_in_out(&inputs, &in_filters);
  convert_filters_in_out(&outputs, &out_filters);

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, in_filters);
  Store_field(ans, 1, _filter_graph);
  Store_field(ans, 2, out_filters);
  CAMLreturn(ans);
}

CAMLprim value graph_config(value _filter_graph)
{
  CAMLparam1(_filter_graph);

  int ret;
  AVFilterGraph *filter_graph = FilterGraph_val(_filter_graph);

  if ((ret = avfilter_graph_config(filter_graph, NULL)) < 0) {
    av_log(NULL, AV_LOG_FATAL,
        "Error initializing complex filters: %s\n",
        av_err2str(ret));
    exit(1);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value graph_dump(value _level, value _filter_graph)
{
  CAMLparam2(_level, _filter_graph);

  int level = Int_val(_level);
  AVFilterGraph *filter_graph = FilterGraph_val(_filter_graph);
  char *dump = avfilter_graph_dump(filter_graph, NULL);

  av_log(NULL, level, "%s\n", dump);
  av_free(dump);

  CAMLreturn(Val_unit);
}

int avfilter_graph_request_oldest(AVFilterGraph *graph)
{
    AVFilterLink *oldest = graph->sink_links[0];
    int64_t frame_count;
    int r;

    while (graph->sink_links_count) {
        oldest = graph->sink_links[0];
        if (oldest->dst->filter->activate) {
            /* For now, buffersink is the only filter implementing activate. */
            r = av_buffersink_get_frame_flags(oldest->dst, NULL,
                                              AV_BUFFERSINK_FLAG_PEEK);
            if (r != AVERROR_EOF)
                return r;
        } else {
            r = ff_request_frame(oldest);
        }
        if (r != AVERROR_EOF)
            break;
        av_log(oldest->dst, AV_LOG_DEBUG, "EOF on sink link %s:%s.\n",
               oldest->dst ? oldest->dst->name : "unknown",
               oldest->dstpad ? oldest->dstpad->name : "unknown");
        /* EOF: remove the link from the heap */
        if (oldest->age_index < --graph->sink_links_count)
            heap_bubble_down(graph, graph->sink_links[graph->sink_links_count],
                             oldest->age_index);
        oldest->age_index = -1;
    }
    if (!graph->sink_links_count)
        return AVERROR_EOF;
    av_assert1(!oldest->dst->filter->activate);
    av_assert1(oldest->age_index >= 0);
    frame_count = oldest->frame_count_out;
    while (frame_count == oldest->frame_count_out) {
        r = ff_filter_graph_run_once(graph);
        if (r == AVERROR(EAGAIN) &&
            !oldest->frame_wanted_out && !oldest->frame_blocked_in &&
            !oldest->status_in)
            ff_request_frame(oldest);
        else if (r < 0)
            return r;
    }
    return 0;
}

CAMLprim value filter_graph_request_oldest(value _filter_graph)
{
  CAMLparam1(_filter_graph);
  CAMLlocal1(ans);
  AVFilterGraph *filter_graph = FilterGraph_val(_filter_graph);

  int ret;

  switch ((ret = avfilter_graph_request_oldest(filter_graph))) {
    case AVERROR(EAGAIN):
      ans = PVV_Again;
      break;

    case 0:
      /* chosen sink buffer populated */
      ans = PVV_Ok;
      break;

    case AVERROR_EOF:
      /* chosen sink buffer reached EOF, but other might have been
       * populated */
      ans = PVV_End_of_file;
      break;

    default:
      av_log(NULL, AV_LOG_FATAL,
          "Unexpected error attempting to filter frames: %s\n",
          av_err2str(ret));
      exit(1);
  }

  CAMLreturn(ans);
}
