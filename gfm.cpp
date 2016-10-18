#include <cstdlib>  //for system
#include <stdio.h>  //for printf
#include <unistd.h> //for sleep

using namespace std;

int main()
{
  system("./gfm7.exe"); 
  printf("End of run!\a\n");
  sleep(1);
  printf(" beep!\a\n");
  sleep(1);
  printf(" beep!\a\n");
  return 0;
}
