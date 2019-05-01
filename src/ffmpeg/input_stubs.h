

/***** Input file *****/

typedef struct InputFile {
  AVFormatContext *ctx;

  AVThreadMessageQueue *in_thread_queue;
  pthread_t thread; /* thread reading from this file */
  int non_blocking; /* reading packets from the thread should not block */
} InputFile;

#define InputFile_val(v) (*(InputFile**)Data_custom_val(v))


/***** Input stream *****/

typedef struct InputStream {
  AVCodecContext *dec_ctx;

  /* predicted dts of the next packet read for this stream or (when there are
   * several frames in a packet) of the next frame in current packet (in AV_TIME_BASE units) */
  int64_t next_dts;

  int64_t next_pts;  ///< synthetic pts for the next decode frame (in AV_TIME_BASE units)

  /* stats */
  // number of frames/samples retrieved from the decoder
  uint64_t frames_decoded;
} InputStream;

#define InputStream_val(v) (*(InputStream**)Data_custom_val(v))
