cargo run --release $(dirname $0)/smallpt.exe | tee image_.ppm
cat image_.ppm | tail -n +2 > image.ppm
rm image_.ppm
