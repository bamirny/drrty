
typedef struct htmlnode{ 
    char *val; 
    struct htmlnode *left; 
    struct htmlnode *right;
} htmlnode; 

#ifndef BUILDNODE_DOT_H
#define BUILDNODE_DOT_H

htmlnode *buildnode(char *string, htmlnode *leftChild, htmlnode *rightChild);

#endif


#ifndef ISELEMENT_DOT_H
#define ISELEMENT_DOT_H

int isElement(char *elem);

#endif

#ifndef PRINTTREE_DOT_H
#define PRINTTREE_DOT_H

void printtree(htmlnode *root);

#endif
