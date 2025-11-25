#!/bin/sh

# Run this script to re-generate the test PNG files from the existing
# data points using the latest CI test image

CI_TEST_IMAGE=savonet/liquidsoap-ci:debian_trixie

cat > plot-script.sh << EOF
#!/bin/sh
apt-get -y update
apt-get -y install gnuplot
cd /tmp/bla
gnuplot -e 'set term png; set output "crossfade-plot.png" ; plot "crossfade-plot.new.txt" using 1:2 with lines title "new track", "crossfade-plot.old.txt" using 1:2 with lines title "old track"'
gnuplot -e 'set term png; set output "autocue-plot.0.png" ; plot "autocue-plot.0.new.txt" using 1:2 with lines title "new track", "autocue-plot.0.old.txt" using 1:2 with lines title "old track"'
gnuplot -e 'set term png; set output "autocue-plot.1.png" ; plot "autocue-plot.1.new.txt" using 1:2 with lines title "new track", "autocue-plot.1.old.txt" using 1:2 with lines title "old track"'
gnuplot -e 'set term png; set output "autocue-plot.2.png" ; plot "autocue-plot.2.new.txt" using 1:2 with lines title "new track", "autocue-plot.2.old.txt" using 1:2 with lines title "old track"'
EOF

trap 'rm -f plot-script.sh' EXIT

chmod +x plot-script.sh

docker run --volume .:/tmp/bla --entrypoint /bin/sh $CI_TEST_IMAGE -- /tmp/bla/plot-script.sh
