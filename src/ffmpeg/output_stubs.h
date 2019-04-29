

/***** Output file *****/

typedef struct OutputFile {
  AVFormatContext *ctx;

  int finished; /* no more packets should be written */
} OutputFile;

#define OutputFile_val(v) (*(OutputFile**)Data_custom_val(v))


/***** Output stream *****/

typedef struct OutputStream {
  AVStream *st;            /* stream in the output file */

  /* dts of the last packet sent to the muxer */
  int64_t last_mux_dts;

  AVCodecContext *enc_ctx;
  AVCodec *enc;

  /* stats */
  // combined size of all the packets written
  uint64_t data_size;
  // number of packets send to the muxer
  uint64_t packets_written;
  // number of frames/samples sent to the encoder
  uint64_t frames_encoded;

  /* packet quality factor */
  int quality;

  /* packet picture type */
  int pict_type;

  int finished; /* no more packets should be written */

  /* frame encode sum of squared error values */
  int64_t error[4];
} OutputStream;

#define OutputStream_val(v) (*(OutputStream**)Data_custom_val(v))
