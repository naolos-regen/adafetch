#include <sys/sysinfo.h>
#include <stdio.h>


int get_sysinfo(struct sysinfo *system_information)
{
   return (sysinfo(system_information));
}

int main(void)
{
   struct sysinfo sysinfo;
   if (get_sysinfo(&sysinfo) == 0)
   {
      printf("cpu uptime  : %ld\n", sysinfo.uptime);
      printf("cpu loads(?):[ %ld , %ld , %ld ]\n", sysinfo.loads[0], sysinfo.loads[1], sysinfo.loads[2]);
      printf("total  ram  : %ld\n", sysinfo.totalram);
      printf("free   ram  : %ld\n", sysinfo.freeram);
      printf("shared ram  : %ld\n", sysinfo.sharedram);
      printf("buffer ram  : %ld\n", sysinfo.bufferram);
      printf("total swap  : %ld\n", sysinfo.totalswap);
      printf("total high  : %ld\n", sysinfo.totalhigh);
      printf("memory unit : %d\n",   sysinfo.mem_unit);
      printf("procs count : %d\n", sysinfo.procs);
      printf("pad(?) unit : %d\n", sysinfo.pad);

      return (1);

   }
   else 
      return (-1);
}
