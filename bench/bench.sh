#! /bin/sh

# Absolute path to the input video file
file="/scratch/input.video"

tmp=$RANDOM
tmpout="/tmp/bench_$tmp"
tmpc="$tmpout.c"

# Absolute path to the reference MPEG-1 video decoder
refdir="/scratch/ref"

gccflagspre="-O1"
gccflagspost="-lm"



echo "### Compiling Feldspar to C..."
cd ../src
stack exec -- runhaskell Main.hs "$file" compile > $tmpc || exit 1

echo "### Compiling C code..."
gcc -std=c99  "$gccflagspre" "$tmpc" -o "$tmpout" "$gccflagspost" || exit 1

echo "### Runnning Feldpar Decoder..."
time $tmpout > /dev/null

echo "### Running Reference Decoder..."
cd $refdir
time ./decode > /dev/null

rm $tmpout
rm $tmpc
