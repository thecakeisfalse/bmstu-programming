
#include "elem.h"
#include <stdlib.h>

struct Elem *searchlist(struct Elem *list, int k) {
    if (list == NULL)
        return NULL;

    if (list->tag == INTEGER && list->value.i == k)
        return list;

    struct Elem * ans = NULL;
    if (list->tag == LIST && (ans = searchlist(list->value.list, k)) != NULL)
        return ans;

    return searchlist(list->tail, k);
}