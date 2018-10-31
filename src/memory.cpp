#ifdef _WIN32 
#include <windows.h>
#elif __linux__
#include "sys/types.h"
#include "sys/sysinfo.h"
#include <unistd.h>
#elif __APPLE__
//#include mac hdr	
#endif

// [[Rcpp::export(name = ".availableRAM")]]
double availableRAM(double defmem) {
	// return available RAM
	double ram;
	#ifdef _WIN32
		MEMORYSTATUSEX statex;
		statex.dwLength = sizeof(statex);
		GlobalMemoryStatusEx(&statex);
		ram = statex.ullAvailPhys;
	#elif __linux__
		struct sysinfo memInfo;
		sysinfo (&memInfo);
		ram = memInfo.freeram;
	#else
		// mac
	    // perhaps use this
		// https://stackoverflow.com/questions/38490320/how-to-query-amount-of-allocated-memory-on-linux-and-osx
		ram = defmem;		
	#endif
	
	return ram;
}

