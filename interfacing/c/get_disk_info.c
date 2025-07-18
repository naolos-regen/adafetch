#include <sys/statvfs.h>
#include <stdlib.h>

struct statvfs get_disk_info(const char *path) {
    struct statvfs stat;
    if (statvfs(path, &stat) != 0)
        exit(EXIT_FAILURE);
    return (stat);
}
