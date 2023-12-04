# Uses the SoX library to create spectrograms of all .WAV files 
# in the test/corpus and test/output directories.
echo "Creating corpus spectrograms.."
for file in ../corpus/*.wav
do
    outfile="${file%.*}.png"
    sox "$file" -n spectrogram -arl -o "$outfile"
    cp "${file%.*}.png" "../analyse/"
done

echo "Creating output spectrograms.."
for file in ../output/*.wav
do
    outfile="${file%.*}.png"
    sox "$file" -n spectrogram -arl -o "$outfile"
    cp "${file%.*}.png" "../analyse/"
done
echo "Done."
