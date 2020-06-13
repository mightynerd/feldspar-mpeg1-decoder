# feldspar-mpeg1-decoder
A partially complete MPEG-1 video decoder written in [Feldspar](https://hackage.haskell.org/package/raw-feldspar) as a Master's Thesis in computer science at Chalmers University of Technology. It should be seen as a proof of concept rather than a usable decoder.

## Compliance and limitations
The decoder can decode MPEG-1 video files with the following limitations (and possibly more):
- The input must be a raw MPEG-1 video stream (not inside any container)
- The stream may only contain I- and P-frames (B- and D-frames are not supported)
- The horizontal resolution must be a multiple of 16.

Usage limitaions of the decoder:
- As Feldspar doesn't support command line arguments nor string manipulation, the input file names must be supplied to the Haskell `main` function or hardcoded in `Main.hs` (see the usage section). This also means that Feldspar has to generate the C source code and compile it every time the input file name is changed.
- The output file format is `YV12` as described [here](https://docs.microsoft.com/en-us/windows/win32/medfound/recommended-8-bit-yuv-formats-for-video-rendering#yv12)
- The output file name is `../v.yuv` unless modified in `Main.hs`
- The decoder does not recover from any errors in the input and crashes even upon successfully completing the decoding process.

## Usage
The following should sucessfully download and build the decoder along with the required libraries:
```
$ git clone https://github.com/mightynerd/feldspar-mpeg1-decoder
$ cd feldspar-mpeg1-decoder
$ stack build
```
Once this has completed, you can use `stack run` to run the decoder:
```
$ stack run -- "INPUT_FILE" run
```
Where the first argument is the input MPEG-1 video file. The second argument may be:
- `run` which generates the C code, compiles it and decodes the input file. 
- `compile` which only outputs the generated C code to stdout.

### Creating input files
To create a input file, use ffmpeg as follows:
```
$ ffmpeg -i INPUT_FILE -f mpeg1video OUTPUT_FILE
```
This will work for any `ÌNPUT_FILE` that your ffmpeg version supports. Note that the decoder is very slow and you might want to rescale it to a lower resolution using the `-s` argument, such as `-s 640x360`.
### Playing back the output files
The output files can be played back with ffplay in the following way:
```
$ ffplay -s WIDTHxHEIGHT FILE_NAME
```
Remember to specify the resolution of the video as the raw output file does not contain this information.

The equivalent but with mplayer:
```
$ mplayer -demuxer rawvideo -rawvideo w=640:h=360:format=i420 FILE_NAME
```
### Generating VLC trees and matrices
The VLC trees and matrices required for the decoding process are stored in their respective folders in plain text format. Their corresponding Haskell representation can be generated using `GenerateVLC.hs` and `GenerateMatrices.hs`.
