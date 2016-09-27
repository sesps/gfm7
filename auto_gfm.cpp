#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstdlib>
#include <iomanip>      // std::setfill, std::setw

using namespace std;

int main()
{
  float energy[99];
  float counts[99];
  char const* fname="temp.lst"; //name of file with list energies and counts
  
  ifstream infile(fname);
  printf("Reading list file \"%s\"\n",fname);
  printf(" The following settings will be used\n");
  int i=0;
  while (infile >> energy[i]) {
    infile >> counts[i];
    printf("  %f %f\n",energy[i],counts[i]);
    i++;
  }
  
  system("> output.mca");
  for(int j=0;j<i;j++)
    {
      std::ofstream outfile("head.dat"); //name of default output file
      outfile << "Iteration " <<j<< " of "<<i<<": energy="<<energy[j]<<" MeV, counts="<<counts[j]<< endl; //set title
      outfile << left << setw(5) << counts[j] +2 << "500  0    3    0    2    0    12   015  NR,NP,NSKIP,JFOCAL,JMTRX,JNR,NPLT,NRXS,LPAX" << endl;
      outfile << left<< setw(10) << energy[j] << "00.  90.0 .0   12.  32.  26.  15        0.0       ENERGY,DEN,TTGT,XNEN,ZTGT,PMASS,ATGT,Q0,DMASS" << endl;
      system("cat head.dat base.dat > input.dat");
      system("./gfm7.exe"); 
      system("cat input.mca >> output.mca");
    }
}
