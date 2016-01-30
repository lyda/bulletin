#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

int
main(int argc, char *argv[])
{
  uint16_t len;
  size_t bytes;
  unsigned char line[0xffff + 2];

  while (1) {
    bytes = fread(&len, 1, sizeof(uint16_t), stdin);
    if (bytes != 2) {
      break;
    }
    if (len % 2 == 1) {
      len++;
    }
    bytes = fread(line, 1, len, stdin);
    if (bytes != len) {
      break;
    }
    line[len] = '\0';
    printf("%s\n", line);
  }

  exit(0);
  return 0;
}

