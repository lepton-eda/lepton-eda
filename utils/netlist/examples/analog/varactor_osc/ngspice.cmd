.control
tran .5n 10u
linearize v(output)
fft v(output)
write frg_fft.raw mag(v(output))
rusage everything
.endc
