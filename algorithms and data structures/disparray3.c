#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct node {
    int key;
    int value;
    struct node *next;
};

typedef void (*function_t)(int, struct node **);

struct command {
    char *name;
    function_t func;
};

struct commands {
    int n;
    struct command **commands;
};

struct commands *InitCommands(void) {
    struct commands *cmds = malloc(sizeof(struct commands));
    cmds->commands = NULL;
    cmds->n = 0;
    return cmds;
}

void DeleteCommands(struct commands *c) {
    for (int i = 0; i < c->n; i++) {
        free(c->commands[i]);
    }
    free(c->commands);
    free(c);
}

void AddCommand(struct commands *c, char *name, function_t func) {
    c->n++;
    c->commands = realloc(c->commands, c->n * sizeof(struct command *));
    c->commands[c->n - 1] = malloc(sizeof(struct command));
    c->commands[c->n - 1]->name = name;
    c->commands[c->n - 1]->func = func;
}

struct node *get(struct node *n, int key) {
    while (n != NULL) {
        if (n->key == key) {
            return n;
        }
        n = n->next;
    }
    return NULL;
}

void delete(struct node *n, int key) {
    while (n != NULL && n->next != NULL) {
        if (n->next->key == key) {
            struct node *temp = n->next;
            n->next = temp->next;
            free(temp);
            break;
        }
        n = n->next;
    }
}

void at(int m, struct node **arr) {
    int key;
    scanf("%d", &key);

    struct node *temp = get(arr[key % m], key);
    if (temp == NULL) {
        printf("0\n");
    } else {
        printf("%d\n", temp->value);
    }
}

void assign(int m, struct node **arr) {
    int key, value;
    scanf("%d %d", &key, &value);

    struct node *n = arr[key % m];

    if (value == 0) {
        delete(n, key);
    } else {
        struct node *m = get(n, key);
        if (m != NULL) {
            m->value = value;
        } else {
            struct node *temp = malloc(sizeof(struct node));
            temp->key = key;
            temp->value = value;
            temp->next = n->next;
            n->next = temp;
        }
    }
}

int main() {
    int m;
    scanf("%d", &m);

    struct node *arr[m];
    for (int i = 0; i < m; i++) {
        arr[i] = malloc(sizeof(struct node));
        arr[i]->next = NULL;
        arr[i]->key = -1;
    }

    struct commands *commands = InitCommands();
    AddCommand(commands, "AT", &at);
    AddCommand(commands, "ASSIGN", &assign);

    char command[10] = {0};
    while (strcmp(command, "END")) {
        scanf("%s", command);
        for (int i = 0; i < commands->n; i++) {
            if (!strcmp(commands->commands[i]->name, command)) {
                commands->commands[i]->func(m, arr);
            }
        }
    }

    DeleteCommands(commands);

    for (int i = 0; i < m; i++) {
        while (arr[i] != NULL) {
            struct node *temp = arr[i];
            arr[i] = temp->next;
            free(temp);
        }
    }
    return 0;
}