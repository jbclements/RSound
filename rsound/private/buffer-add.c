void bufferAdd(short *dst, short *src, int len) {
  int i;
  for (i = 0; i<len; i++) {
    dst[i] += src[i];
  }
}

void bufferMultAdd(short *dst, short* src, int len, double factor){
  int i;
  for (i = 0; i<len; i++) {
    dst[i] += (short)(factor * ((double)src[i]));
  }
}

