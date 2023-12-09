echo "Creating composite result files.."
for file in *.png
do
    outfile="${file%.*}_out.png"
    resultFile="${file%.*}_out_compare.png"
    composite -verbose "$file" "$outfile" -compose difference "$resultFile"
done
