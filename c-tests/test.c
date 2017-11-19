#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <stdio.h> 
#include <errno.h>
#include <string.h>


int main()
{
  printf("works\n");
        int file=0;
        if((file=open("/proc/8104/mem",O_RDONLY)) < -1)
                return 1;
 
	/* long int index = 0x560851b863e8 + 3; */
	long int index = -1;

        char data[8];
        /* if(read(file,buffer,19) != 19)  return 1; */
        /* printf("%s\n",buffer); */
 
	// off_t lseek(int fildes, off_t offset, int whence);
        if(lseek(file,index,SEEK_SET) < 3) {
	  printf("< 3 strerror: %s\n", strerror(errno));
	  return 1;
	}
 
        /* if(read(file, data,1) != 1)  return 1; */
	data[0] = 0xab;
	data[1] = 0xdd;

	read(file, data,9);
        printf("read(...) strerror: %s\n", strerror(errno));

	printf("index: %llx\n", index);
  
        printf("data[0]: %x\n",data[0]);
        printf("data[1]: %x\n",data[1]);
        printf("data[2]: %x\n",data[2]);
        printf("data[3]: %x\n",data[3]);
        printf("data[4]: %x\n",data[4]);
        printf("data[5]: %x\n",data[5]);
        printf("data[6]: %x\n",data[6]);
        printf("data[7]: %x\n",data[7]);
	printf("full:%02x%02x%02x%02x%02x%02x%02x%02x\n",
	       data[7],data[6],data[5],data[4],
	       data[3],data[2],data[1],data[0]);
        return 0;
}
