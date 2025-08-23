# Adafetch

The basic idea of Adafetch is to create my own configurable fetch sysinfo.


# Informations-For-Me

## Info-Linux-Sys



This might be a bit too much, but Hell-yeah!
```bash
# Uptime-Info
>$ cat /proc/uptime

# OS-Info
>$ cat /etc/os-release
>$ cat /proc/version

# CPU-Info
>$ cat /proc/cpuinfo
>$ cat /proc/stat

# RAM-Info
>$ cat /proc/meminfo

# GPU-Info
>$ cat /sys/class/graphics/fb0/device/ device | vendor
>$ cat /sys/class/card/drm/card<n>/device/

# Battery-Info
>$ cat /sys/class/power_supply/BAT0

# Monitor-Brighntess
>$ cat /sys/class/backlight/<ALL>/max_brighntess | actual_brightness

# bios
>$ cat /sys/devices/virtual/dmi/id/bios_*

```

## Posix-Information

```bash
statvfs
getuid
geteuid
getpwuid
getlogin
getenv
uname
sysconf
isatty
ioctl
sysconf
gethostname
getenv
time
localtime
strftime
clock_gettime


unistd.h 
pwd.h 
utsname.h 
statvfs.h 
types.h 
ioctl.h 
time.h 
locale.h 
stdlib.h
```
