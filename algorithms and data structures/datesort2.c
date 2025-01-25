#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Date
{
  int Day, Month, Year;
} Date;

int main()
{
  int n;
  scanf("%d", &n);

  Date* dates = (Date*)calloc(n, sizeof(Date));

  for (int i = 0; i < n; i++)
  {
    Date *d = &dates[i];
    scanf("%d %d %d", &d->Year, &d->Month, &d->Day);
  }

  Date* substitute = (Date*)calloc(n, sizeof(Date));

  int Days[32] = {0}, Months[13] = {0}, Years[61] = {0};

  for (int i = 0; i < n; i++)
  {
    Days[dates[i].Day]++;
  }
  for (int i = 1; i <= 31; i++)
  {
    Days[i] += Days[i-1];
  }
  for (int i = n-1; i >= 0; i--)
  {
    substitute[Days[dates[i].Day]-1] = dates[i];
    Days[dates[i].Day]--;
  }

  for (int i = 0; i < n; i++)
  {
    Months[substitute[i].Month]++;
  }
  for (int i = 1; i <= 12; i++)
  {
    Months[i] += Months[i-1];
  }
  for (int i = n-1; i >= 0; i--)
  {
    dates[Months[substitute[i].Month]-1] = substitute[i];
    Months[substitute[i].Month]--;
  }

  for (int i = 0; i < n; i++)
  {
    Years[dates[i].Year-1970]++;
  }
  for (int i = 1; i <= 60; i++)
  {
    Years[i] += Years[i-1];
  }
  for (int i = n-1; i >= 0; i--)
  {
    substitute[Years[dates[i].Year-1970]-1] = dates[i];
    Years[dates[i].Year-1970]--;
  }

  for (int i = 0; i < n; i++)
  {
    Date* d = &substitute[i];
    printf("%04d %02d %02d\n", d->Year, d->Month, d->Day);
  }

  free(substitute);
  free(dates);

  return 0;
}
