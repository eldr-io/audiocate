echo "Creating composite result files.."
for file in test/analyse/*.png
do
    outfile="${file%.*}_out.png"
    resultFile="${file%.*}_out_compare.png"
    magick compare -verbose -metric mse "$file" "$outfile" "$resultFile"
done
