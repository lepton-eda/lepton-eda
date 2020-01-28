.control
tran .5n 10u
linearize v(output)
fft v(output)
write frg_fft.raw mag(v(output))
.endc

notes:

FFT does a fft analysis on the vecs spicified. This is of course complex and 
requires mag, just like bandpass example. You and also just look at transient 
analysis which should be REAL and see the wave form.
buth can be viewed with gaw, gwave or built in viewer in ngpsice.

