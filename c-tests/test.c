#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include<stdio.h> 

int main()
{
  printf("works\n");
        int file=0;
        if((file=open("/proc/12194/mem",O_RDONLY)) < -1)
                return 1;
 
	long long int index = 0x1db2000;
        int data[1];
        /* if(read(file,buffer,19) != 19)  return 1; */
        /* printf("%s\n",buffer); */
 
	// off_t lseek(int fildes, off_t offset, int whence);
        if(lseek(file,index,SEEK_SET) < 0) return 1;
 
        /* if(read(file, data,1) != 1)  return 1; */
	data[0] = 0xabcd;
	data[1] = 0xba;

	read(file, data,8);
	printf("index: %llx\n", index);
  
        printf("data[0]: %x\n",data[0]);
        printf("data[1]: %x\n",data[1]);

 
        return 0;
}
