/* DRRTY standard library */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//Linked List Implementation

struct node{
    void * value;
    struct node * next;
};

struct list{
    int size;
    struct node * head;
};

//Initialize list
struct list* list_init(){

	struct list *l;
	l = malloc(sizeof(struct list));

	if (l == NULL)
		return NULL;

	l->size = 0;
    l->head = 0;

	return l;
}

//Return length of list
int list_size(struct list *l){

	return l->size;
}

//Return element at index i of list
void * list_get(struct list *l, int i) {

	if (l->head == NULL || l->size <= i || i < 0)
		return NULL;

	struct node *current = l->head;

	int j = 0;
	while (j != i) {
		current = current->next;
                j++;
	}

	return current->value;
}

//Set value at index i of list
int list_set(struct list *l, int i, void *value){

	if (l->head == NULL || l->size <= i || i < 0)
		return 0;

	struct node *current = l->head;

	int j = 0;
	while (j != i) {
		current = current->next;
        j++;
	}

	current->value = value;
	return 1;
}

//Add element to end of list
int list_add(struct list *l, void *value){

    struct node *new = (struct node *)malloc(sizeof(struct node));
	
    if (new == NULL)
		return 0;

	new->value = value;
	new->next = NULL;
	
    if (l->head == NULL) {
        l->size +=  1;
	    l->head = new;
		return 1;
    }

	struct node *current = l->head;
	while (current->next != NULL) {
		current = current->next;
	}

	current->next = new;
	l->size += 1;
	return 1;
}

void * list_pop(struct list *l) {

	if (l->head == NULL)
		return NULL;

	if (l->head->next == NULL) {
		struct node *old = l->head;
	    l->head = old->next;
	    void *value = old->value;
	    free(old);
	    l->size -= 1;
	    return value;
        }

	struct node *temp1 = l->head;
	struct node *temp2 = l->head->next;

	while (temp2->next != NULL) {
		temp1 = temp2;
		temp2 = temp2->next;
	}

	void *value = temp2->value;
	temp1->next = NULL;
	l->size -= 1;
	free(temp2);
	return value;
}

void printl(struct list *l) {

	printf("[");

	struct node *current = l->head;
	while (current != NULL){
            if(current->next == NULL)
                printf("%d", *(int *) current->value);
            else
                printf("%d,", *(int *)current->value);
    
            current = current->next;
	}
	printf("]\n");
}

// Casting types into generics

// From Integer
int list_add_int (struct list * l, int value)
{
    int * d = malloc(sizeof(int));
    *d = value;
    return list_add(l, d);
}

int list_get_int(struct list * l, int index)
{
    void * answer = list_get(l, index);
    return *(int *) answer;
}

int list_set_int(struct list * l, int index, int x)
{
    int answer = list_get_int(l, index);
    int * d = malloc(sizeof(int));
    *d = x;
    list_set(l, index, (void *) d);
    return answer;
}

int list_pop_int(struct list * l)
{
    void * answer = list_pop(l);
    return *(int *) answer;
}

/* From Float */
int list_add_float (struct list * l, double value)
{
    double * d = malloc(sizeof(double));
    *d = value;
    return list_add(l, d);
}

double list_get_float(struct list * l, int index)
{
    void * answer = list_get(l, index);
    return *(double *) answer;
}

double list_set_float(struct list * l, int index, double x)
{
    double answer = list_get_float(l, index);
    double * d = malloc(sizeof(double));
    *d = x;
    list_set(l, index, (void *) d);
    return answer;
}

double list_pop_float(struct list * l)
{
    void * answer = list_pop(l);
    return *(double *) answer;
}

/* From String */
int list_add_str (struct list * l, char * value)
{
    return list_add(l, (void *) value);
}

char * list_get_str(struct list * l, int index)
{
    void * answer = list_get(l, index);
    return (char *) answer;
}

char * list_set_str(struct list * l, int index, char * x)
{
    char * answer = list_get_str(l, index);

    list_set(l, index, (void *) x);
    return answer;
}

char * list_pop_str(struct list * l)
{
    void * answer = list_pop(l);
    return (char *) answer;
}