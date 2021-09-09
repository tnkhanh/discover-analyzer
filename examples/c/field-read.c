#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

struct node {
    int val;
    struct node* next;
    struct node* prev;
};

int main(int arg) {
    struct node* a = malloc(sizeof(struct node));
    a->val = 10;
    a->next = NULL;
    a->prev = NULL;
}
