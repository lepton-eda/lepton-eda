for dev in and nand or nor xor xnor
do
  for i in 2 3 4 5 6 7 8 9
  do
    sed -f script.sed <../verilog/$dev$i-1.sym |sed -e s/device=$dev/device=$dev$i/ >$dev$i-1.sym
  done
done

