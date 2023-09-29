#!/usr/bin/env bash
cd ~
echo "Playlist number"
read playnum

# Checks for wsl
echo "Are you in wsl? (y/n)"
read wsl

# Create directories
mkdir -p Videos/Playlist\ $playnum
cd Videos/Playlist\ $playnum
mkdir L J Y Final

# Read Links from stdin
echo "J Link"
read link1 

echo "L Link"
read link2

echo "Y Link"
read link3

# Download playlists from each link
cd J 
yt-dlp -o "%(playlist_index)02dA.%(ext)s" $link1

cd ../L
yt-dlp -o "%(playlist_index)02dB.%(ext)s" $link2

cd ../Y
yt-dlp -o "%(playlist_index)02dC.%(ext)s" $link3

# Copy all playlist files into the final folder
cd ../Final
cp ../J/* .
cp ../L/* .
cp ../Y/* .

# Create a concat list for ffmpeg
for f in *.webm; do echo "file $f'" >> mylist.txt; done

case $wsl in
    y ) export LD_LIBRARY_PATH="/usr/lib/wsl/lib"
	ffmpeg -hwaccel cuda -f concat -i mylist.txt -c:v h264_nvenc -crf 23 -s 1920x1080 "Playlist_$playnum.mp4" ;;
    n ) 
	ffmpeg -hwaccel cuda -f concat -i mylist.txt -c:v h264_nvenc -crf 23 -s 1920x1080 "Playlist_$playnum.mp4" ;;
esac