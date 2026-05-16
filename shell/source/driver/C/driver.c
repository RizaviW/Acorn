#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/vfio.h>

typedef struct {
    int container_descriptor;
    int group_descriptor;
    int device_descriptor;
    volatile uint32_t *bar0;
    size_t bar0_size;
} system_context;

int setup_fpga_driver(system_context *context, int group_identifier, const char *device_identifier) {
    char path[64];
    struct vfio_group_status group_status = { .argsz = sizeof(group_status) };
    struct vfio_region_info region_info = { .argsz = sizeof(region_info) };

    context->container_descriptor = open("/dev/vfio/vfio", O_RDWR);
    if (context->container_descriptor < 0) {
        perror("Container open failed");
        return -1;
    }

    if (ioctl(context->container_descriptor, VFIO_CHECK_EXTENSION, VFIO_TYPE1_IOMMU) != 1) {
        close(context->container_descriptor);
        return -1;
    }

    snprintf(path, sizeof(path), "/dev/vfio/%d", group_identifier);
    context->group_descriptor = open(path, O_RDWR);
    if (context->group_descriptor < 0) {
        perror("Group open failed");
        close(context->container_descriptor);
        return -1;
    }

    if (ioctl(context->group_descriptor, VFIO_GROUP_SET_CONTAINER, &context->container_descriptor) < 0) {
        perror("Binding group to container failed");
        close(context->group_descriptor);
        close(context->container_descriptor);
        return -1;
    }

    if (ioctl(context->container_descriptor, VFIO_SET_IOMMU, VFIO_TYPE1_IOMMU) < 0) {
        perror("Setting IOMMU type failed");
        close(context->group_descriptor);
        close(context->container_descriptor);
        return -1;
    }

    context->device_descriptor = ioctl(context->group_descriptor, VFIO_GROUP_GET_DEVICE_FD, device_identifier);
    if (context->device_descriptor < 0) {
        perror("Device descriptor retrieval failed");
        close(context->group_descriptor);
        close(context->container_descriptor);
        return -1;
    }

    region_info.index = VFIO_PCI_BAR0_REGION_INDEX;
    if (ioctl(context->device_descriptor, VFIO_DEVICE_GET_REGION_INFO, &region_info) < 0) {
        perror("BAR0 region info retrieval failed");
        close(context->device_descriptor);
        close(context->group_descriptor);
        close(context->container_descriptor);
        return -1;
    }

    context->bar0_size = region_info.size;

    void *map_base = mmap(
        NULL, 
        region_info.size, 
        PROT_READ | PROT_WRITE, 
        MAP_SHARED, 
        context->device_descriptor, 
        region_info.offset
    );

    if (map_base == MAP_FAILED) {
        perror("BAR0 mmap failed");
        close(context->device_descriptor);
        close(context->group_descriptor);
        close(context->container_descriptor);
        return -1;
    }

    context->bar0 = (volatile uint32_t *)map_base;
    return 0;
}

void cleanup_driver(system_context *context) {
    if (context->bar0 && context->bar0 != MAP_FAILED) {
        munmap((void *)context->bar0, context->bar0_size);
    }
    if (context->device_descriptor >= 0) close(context->device_descriptor);
    if (context->group_descriptor >= 0) close(context->group_descriptor);
    if (context->container_descriptor >= 0) close(context->container_descriptor);
}

int main(int argc, char const *argv[]) {
    system_context context = { .bar0 = NULL, .device_descriptor = -1, .group_descriptor = -1, .container_descriptor = -1 };
    
    int group_identifier = 24;
    const char *device_identifier = "0000:09:00.0";

    if (setup_fpga_driver(&context, group_identifier, device_identifier) != 0) {
        return EXIT_FAILURE;
    }

    // Direct register access via BAR0 mapping
    printf("Driver active. BAR0 mapped at %p\n", (void*)context.bar0);

    cleanup_driver(&context);
    return EXIT_SUCCESS;
}
