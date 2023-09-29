#!/usr/bin/env bash
cd ~
echo "Playlist number"
read playnum

# Checks for wsl
echo "Are you in wsl? (y/n)"
read $wsl

# Create directories
mkdir -p Videos/Playlist\ $playnum
cd Vides/Playlist\ $playnum
mkdir Livia Jorn Yves Final

# Read Links from stdin
echo "Jorn Link"
read link1 

echo "Livia Link"
read link2

echo "Yves Link"
read link3

# Download playlists from each link
cd Jorn 
yt-dlp -o "%(playlist_index)02dA.%(ext)s" $link1

cd ../Livia
yt-dlp -o "%(playlist_index)02dB.%(ext)s" $link2

cd ../Yves
yt-dlp -o "%(playlist_index)02dC.%(ext)s" $link3

# Copy all playlist files into the final folder
cd ../Final
cp ../Jorn/* .
cp ../Livia/* .
cp ../Yves/* .

# Create a concat list for ffmpeg
for f in *.webm; do echo "file $f'" >> mylist.txt; done

case $wsl in
    y ) export LD_LIBRARY_PATH="/usr/lib/wsl/lib"
	ffmpeg -hwaccel cuda -f concat -i mylist.txt -c:v h264_nvenc -crf 23 -s 1920x1080 "Playlist_$playnum.mp4" ;;
    n ) 
	ffmpeg -hwaccel cuda -f concat -i mylist.txt -c:v h264_nvenc -crf 23 -s 1920x1080 "Playlist_$playnum.mp4" ;;
esac
