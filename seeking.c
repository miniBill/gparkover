#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

static char * sizes[] = { "c", "K", "M", "G", "T", "P" };

static void printSize2 (off_t size, int s)
{
	off_t next = size / 1024;
	if (next > 0)
		printSize2 (next, s + 1);
	printf ("%llu%sB ", size % 1024, sizes[s]);
}

static void printSize (off_t size)
{
	printSize2 (size, 0);
}

static void runAdvice2 (char * broken, off_t start, off_t length, long long displacement, int s)
{
	if (start % 1024 == 0 && length % 1024 == 0 && displacement % 1024 == 0)
	{
		runAdvice2 (broken, start / 1024, length / 1024, displacement / 1024, s + 1);
		return;
	}
	printf ("dd if=%s of=rebuilt.img bs=1%s count=%llu\n", broken, sizes [s], start);
	printf ("dd if=%s of=rebuilt.img bs=1%s skip=%llu seek=%llu\n", broken, sizes [s], start + length, start);
}

static void runAdvice (char * broken, long long i, off_t length, long long displacement)
{
	if (length < displacement/2)
		return;
	off_t start = i - length - displacement;
	printf ("Match found at ");
	printSize (start);
	printf ("of length ");
	printSize (length);
	printf ("\n");
	printf ("Run this:\n");
	runAdvice2 (broken, start, length, displacement, 0);
}

int main (int argc, char ** argv)
{
	if (argc < 3)
	{
		fprintf (stderr, "Usage: %s filename offset\n", argv[0]);
		return 1;
	}

	struct stat sb;
	if (stat (argv [1], &sb) == -1)
	{
		perror ("stat");
		return 1;
	}

	long long filesize = sb.st_size;
	int fd = open (argv [1], O_RDONLY);
	char * file = mmap (NULL, filesize, PROT_READ, MAP_SHARED | MAP_POPULATE, fd, 0);
	off_t length = 0;
	long long displacement = atoll (argv[2]);
	for (long long i = displacement; i < filesize; i++)
	{
		if (file[i - displacement] == file[i])
		{
			length++;
			continue;
		}
		if (length > displacement/2)
			runAdvice (argv[1], i, length, displacement);
		length = 0;
	}
	runAdvice (argv[1], filesize, length, displacement);
	return 0;
}
