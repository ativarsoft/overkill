#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

int check_extension(const char *exts, const char *filename_extension)
{
	const char *s = exts;
	char *dup;
	const char *token;
	const char separator[] = ";";
	while (*s != '\0') {
		dup = strdup(s);
		token = strtok(dup, separator);
		while (token != NULL) {
			printf("Testing %s with %s extension.\n",
				filename_extension, token);
			if (strcasecmp (token, filename_extension) == 0) {
				free(dup);
				return 0;
			}
			token = strtok(NULL, separator);
		}
		s += strlen (s) + 1;
		s += strlen (s) + 1;
		free(dup);
	}
	return 1;
}

