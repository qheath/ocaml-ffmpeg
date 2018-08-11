OCaml interface for the [FFmpeg](http://ffmpeg.org/) Multimedia framework.


Upstream
========

[savonet/ocaml-ffmpeg](https://github.com/savonet/ocaml-ffmpeg/)


Modules
=======

The modules currently available are :
- `Av`: demuxers and muxers for reading and writing multimedia container
  formats
- `Avcodec`: decoders and encoders for audio, video and subtitle codecs
- `Swresample`: audio resampling, rematrixing and sample format
  conversion operations
- `Swscale`: image scaling and color space/pixel format conversion
  operations
- `Avdevice`: input and output devices for grabbing from and rendering
  to many common multimedia input/output software frameworks

Please read the COPYING file before using this software.


Prerequisites
=============

- ocaml >= 4.05.0
- FFmpeg >= 4.0
- findlib >= 0.8.1


Compilation
===========

    $ dune build @install


Documentation
=============

The [API documentation is available here](https://www.liquidsoap.info/ocaml-ffmpeg/docs/api/index.html).


Examples
========

- [audio_decoding](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/audio_decoding/audio_decoding.ml):
  read frames from an audio file and convert them into bytes
- [audio_device](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/audio_device/audio_device.ml):
  read 500 audio frames from an input audio device or an URL and write
  them into an output audio device or a file
- [decode_audio](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/decode_audio/decode_audio.ml):
  parse packets from a mapped file, decode them and write the resulting
  frames into a file
- [demuxing_decoding](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/demuxing_decoding/demuxing_decoding.ml):
  demuxing and decoding audio, video and subtitle frames from a file,
  converts them into bytes and write them in raw files
- [encode_audio](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/encode_audio/encode_audio.ml):
  convert a float array into stereo frames and encode them into packets
- [encode_video](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/encode_video/encode_video.ml):
  create video frames and write them encoded into a file
- [encoding](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/encoding/encoding.ml):
  create a multimedia file with audio and video streams
- [player](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/player/player.ml):
  read a multimedia file and write audio and video frames to output
  devices
- [remuxing](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/remuxing/remuxing.ml):
  remuxing multimedia file packets without decoding them
- [transcode_aac](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/transcode_aac/transcode_aac.ml):
  transcode an audio file into an AAC audio file
- [transcoding](https://github.com/qheath/ocaml-ffmpeg/blob/master/examples/transcoding/transcoding.ml):
  transcode audio streams into the AAC codec, video streams into the
  H264 codec and write them to an output file
