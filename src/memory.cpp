#ifdef _WIN32 
#include <windows.h>
#elif __linux__
#include "sys/types.h"
#include "sys/sysinfo.h"
#include <unistd.h>
#elif __APPLE__
#include <mach/vm_statistics.h>
#include <mach/mach_types.h>
#include <mach/mach_init.h>
#include <mach/mach_host.h>
#endif

// [[Rcpp::export(name = ".availableRAM")]]
double availableRAM(double ram) {
	// return available RAM
	#ifdef _WIN32
		MEMORYSTATUSEX statex;
		statex.dwLength = sizeof(statex);
		GlobalMemoryStatusEx(&statex);
		ram = statex.ullAvailPhys;
	#elif __linux__
		struct sysinfo memInfo;
		sysinfo (&memInfo);
		ram = memInfo.freeram;
	#elif __APPLE__
		vm_size_t page_size;
		mach_port_t mach_port;
		mach_msg_type_number_t count;
		vm_statistics64_data_t vm_stats;

		mach_port = mach_host_self();
		count = sizeof(vm_stats) / sizeof(natural_t);
		if (KERN_SUCCESS == host_page_size(mach_port, &page_size) &&
			KERN_SUCCESS == host_statistics64(mach_port, HOST_VM_INFO,
											(host_info64_t)&vm_stats, &count)) {
			long long free_memory = ((int64_t)vm_stats.free_count +
                               (int64_t)vm_stats.inactive_count) * (int64_t)page_size;
			ram = free_memory;
		//https://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process
		}
	#endif
	
	return ram;
}

