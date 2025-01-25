#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 100000

typedef char* const string;

typedef struct node
{
  struct node* next[26];
  int count;
  bool terminal;
} TrieNode;

TrieNode* newTrieNode()
{
  TrieNode* node = (TrieNode*)malloc(sizeof(TrieNode));
  node->count = 0;
  node->terminal = false;
  for (int i = 0; i < 26; i++)
  {
    node->next[i] = NULL;
  }
  return node;
}

void deleteTrieNode(TrieNode* node)
{
  if (node != NULL)
  {
    for (int i = 0; i < 26; i++)
    {
      deleteTrieNode(node->next[i]);
    }
  }
  free(node);
}

typedef struct
{
  TrieNode* root;
} Trie;

Trie* newTrie()
{
  Trie* trie = (Trie*)malloc(sizeof(Trie));
  trie->root = newTrieNode();
  return trie;
}

void deleteTrie(Trie* trie)
{
  if (trie != NULL)
  {
    deleteTrieNode(trie->root);
  }
  free(trie);
}

bool Contains(Trie* trie, string word)
{
  TrieNode* v = trie->root;
  const int size = strlen(word);

  for (int i = 0; i < size; i++)
  {
    const int u = word[i]-'a';
    if (!v->next[u])
    {
      return false;
    }
    v = v->next[u];
  }

  return v->terminal;
}

void Insert(Trie* trie, string word)
{
  TrieNode* v = trie->root;
  const int size = strlen(word);

  for (int i = 0; i < size; i++)
  {
    v->count++;
    const int u = word[i]-'a';
    if (!v->next[u])
    {
      v->next[u] =  newTrieNode();
    }
    v = v->next[u];
  }
  v->terminal = true;
  v->count++;
}

void Delete(Trie* trie, string word)
{
  TrieNode* v = trie->root;
  const int size = strlen(word);

  for (int i = 0; i < size; i++)
  {
    v->count--;
    const int u = word[i]-'a';
    if (!v->next[u])
    {
      break;
    }
    v = v->next[u];
  }
  v->terminal = false;
  v->count--;
}

int Prefix(Trie* trie, string word)
{
  TrieNode* v = trie->root;
  const int size = strlen(word);

  for (int i = 0; i < size; i++)
  {
    const int u = word[i]-'a';
    if (!v->next[u])
    {
      return 0;
    }
    v = v->next[u];
  }

  return v->count;
}

int main()
{
  Trie* trie = newTrie();

  string buffer = malloc(2*MAX_SIZE);
  string command = malloc(10);
  string input = malloc(MAX_SIZE);

  while (1)
  {
    fgets(buffer, 2*MAX_SIZE, stdin);

    sscanf(buffer, "%s %s\n", command, input);
    
    if (!strcmp(command, "INSERT"))
    {
      if (!Contains(trie, input))
      {
        Insert(trie, input);
      }
    }
    else if (!strcmp(command, "DELETE"))
    {
      if (Contains(trie, input))
      {
        Delete(trie, input);
      }
    }
    else if (!strcmp(command, "PREFIX"))
    {
      const int answ = Prefix(trie, input);
      printf("%d\n", answ);
    }
    else if (!strcmp(command, "END"))
    {
      break;
    }
  }

  deleteTrie(trie);

  free(buffer);
  free(command);
  free(input);

  return 0;
}
