#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstdlib>  //for system
#include <iomanip>  //file formatting; std::setfill, std::setw
#include <unistd.h> //for sleep

using namespace std;

int main()
{
  float energy[999];
  float counts[999];
  char const* fname="rand.lst"; //name of file with list energies and counts
  
  ifstream infile(fname);
  printf("Reading list file \"%s\"\n",fname);
  printf(" The following settings will be used\n");
  int i=0;
  int max=100;
  while (infile >> energy[i]) {
    infile >> counts[i];
    if(counts[i]>max)
      counts[i]=max;
    printf("  %f %f\n",energy[i],counts[i]);
    i++;
  }

  float pres=2.0;
  //           GASOPT---|ACAPT----|TFWOPT---|
  string gas2="-0.074    0.153     -1        GAS,QOPT,AGAS,TFOIL,ZGAS,FOIL,ZION,PRESS,GASOPT,ACAPT,TFWOPT";  
  system("> output.mca");
  for(int j=0;j<i;j++)
    {
      std::ofstream outfile("head.dat"); //name of default output file
      outfile << "Iteration " <<j+1<< " of "<<i<<": energy="<<energy[j]<<" MeV, counts="<<counts[j]<< endl; //set title
      //                                        NR--|NP--|NSKI|JFOC|JMTR|JNR-|NPLT|NRXS|LPAX|
      outfile << left << setw(5) << counts[j] +2 << "500  0    3    0    2    0    12   015  NR,NP,NSKIP,JFOCAL,JMTRX,JNR,NPLT,NRXS,LPAX" << endl; //make no adjustments
      //                                        ENERGY---|DEN-|TTGT|XNEN|ZTGT|PMAS|ATGT|Q0-------|DMASS----|
      outfile << left<< setw(10) << energy[j] <<         "00.  90.0 .0   12.  38.9626.  0.        0.0       ENERGY,DEN,TTGT,XNEN,ZTGT,PMASS,ATGT,Q0,DMASS" << endl; //set vales; q0 must be zero for vacuum mode
      //          GAS-|QOPT|AGAS|TFOI|ZGAS|FOIL|ZION-----|PRESS----|GASOPT---|ACAPT----|TFWOPT---|
      outfile << "1.   1.   4.   .001 2.   My   17.       "<< left<< setw(10) << fixed << pres<< gas2 << endl;
      system("cat head.dat base.dat > input.dat");
      system("./gfm7.exe"); 
      system("cat input.mca >> output.mca");
    }

  std::ofstream outfile("head.dat"); //name of default output file
  outfile << "Beam"<< endl; //set title
  //                                        NR--|NP--|NSKI|JFOC|JMTR|JNR-|NPLT|NRXS|LPAX|
  outfile << left << setw(5) << 3000 +2 << "500  0    3    0    2    0    12   015  NR,NP,NSKIP,JFOCAL,JMTRX,JNR,NPLT,NRXS,LPAX" << endl; //make no adjustments
  //          ENERGY---|DEN-|TTGT|XNEN|ZTGT|PMAS|ATGT|Q0-------|DMASS----|
  outfile << "20.0      00.  90.0 .0   12.  14.0 26.  0.        0.0       ENERGY,DEN,TTGT,XNEN,ZTGT,PMASS,ATGT,Q0,DMASS" << endl; //set vales; q0 must be zero for vacuum mode
  //          GAS-|QOPT|AGAS|TFOI|ZGAS|FOIL|ZION-----|PRESS----|GASOPT---|ACAPT----|TFWOPT---|
  outfile << "1.   1.   4.   .001 2.   My   6.        "<< left<< setw(10) << fixed << pres<< gas2 << endl;
  system("cat head.dat base.dat > input.dat");
  system("./gfm7.exe"); 
  system("cat input.mca >> output.mca");
  
  printf("End of runs!\a\n");
  sleep(1);
  printf(" beep!\a\n");
  sleep(1);
  printf(" beep!\a\n");
  return 0;
}
