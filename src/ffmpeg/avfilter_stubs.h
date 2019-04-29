

/***** FilterInOut *****/

#define FilterInOut_val(v) (*(AVFilterInOut**)Data_custom_val(v))


/***** Filter *****/

typedef struct Filter {
  AVFilterContext *filter_ctx;
  uint8_t         *name;
} Filter;

#define Filter_val(v) (*(Filter**)Data_custom_val(v))

char *describe_filter_link(AVFilterContext *ctx, int pad_idx, int in);

Filter * alloc_filter(value *pvalue);


/***** AVFilterGraph *****/

#define FilterGraph_val(v) (*(AVFilterGraph**)Data_custom_val(v))
