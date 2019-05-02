

/***** Output file *****/

typedef struct OutputFile {
  AVFormatContext *ctx;
} OutputFile;

#define OutputFile_val(v) (*(OutputFile**)Data_custom_val(v))


/***** Output stream *****/

typedef struct OutputStream {
  /* dts of the last packet sent to the muxer */
  int64_t last_mux_dts;

  AVCodecContext *enc_ctx;

  /* packet quality factor */
  int quality;

  /* packet picture type */
  int pict_type;

  /* frame encode sum of squared error values */
  int64_t error[4];
} OutputStream;

#define OutputStream_val(v) (*(OutputStream**)Data_custom_val(v))
