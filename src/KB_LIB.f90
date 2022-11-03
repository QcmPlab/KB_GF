MODULE KB_LIB
  USE KB_VARS_GLOBAL
  USE KB_CONTOUR
  USE KB_AUX, &
       kb_fft_iw2tau=>fft_iw2tau,  &
       kb_fft_tau2iw=>fft_tau2iw,  &
       kb_fft_extract_tau=> fft_extract_gtau
  USE KB_GF_MAIN
END MODULE KB_LIB
