#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/vfio.h>

typedef struct {
  int container_descriptor;
  int group_descriptor;
  int device_descriptor;
} system_context;

int open_container(system_context *context) {
  int descriptor = open("/dev/vfio/vfio", O_RDWR);
  if (descriptor < 0) {
    perror("Failed to open VFIO container");
    return -1;
  } else {
    if (ioctl(descriptor, VFIO_CHECK_EXTENSION, VFIO_TYPE1_IOMMU) != 1) {
      perror("IOMMU Type 1 not supported");
      close(descriptor);
      return -1;
    }
    context -> container_descriptor = descriptor;
    printf("Container opened: %d\n", descriptor);
    return 0;
  }
}

int main(int argc, char const *argv[]) {
  system_context context;
  if (open_container(&context) == 0) {
    return EXIT_SUCCESS;
  } else {
    return EXIT_FAILURE;
  }
}
