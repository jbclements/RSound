void bufferAdd(short *dst, short *src, int len) {
  int i;
  for (i = 0; i<len; i++) {
    dst[i] += src[i];
  }
}
