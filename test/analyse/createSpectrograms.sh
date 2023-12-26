# Uses the SoX library to create spectrograms of all .WAV files 
# in the test/corpus and test/output directories.
echo "Creating corpus spectrograms.."
for file in test/corpus/*.wav
do
    outfile="${file%.*}.png"
    sox "$file" -n spectrogram -arl -o "$outfile"
    cp "${file%.*}.png" "test/analyse/"
done

echo "Creating output spectrograms.."
for file in test/output/*.wav
do
    outfile="${file%.*}.png"
    sox "$file" -n spectrogram -arl -o "$outfile"
    cp "${file%.*}.png" "test/analyse/"
done
rm test/corpus/*.png
rm test/output/*.png
echo "Done."
