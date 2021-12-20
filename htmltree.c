#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <stdbool.h>
#include "htmlparser.h"


htmlnode *buildnode(char *string, htmlnode *leftChild, htmlnode *rightChild){ 
    // make a copy of the string to add null terminal and store in heap
    unsigned long stringLength = strlen(string) + 1;  
    //store copied string in heap
    char *memptr = malloc(stringLength);
    for (int i = 0; i < stringLength; i ++){ 
        *(memptr + i) = *(string + i); 
    }

    //create memory for the struct
    htmlnode *result = malloc( sizeof(htmlnode) ); 
    if( result != NULL){ 
        result -> val = memptr;

        if( !leftChild ){ 
            result -> left = NULL;
        }else{
            result -> left = leftChild;
        }
        if( !rightChild ){ 
            result -> right = NULL;
        }else{
            result -> right = rightChild;
        }
    }
    return result; 
}

int isElement(char *elem){
    
    int res = 0; //false
    char *htmlelements[110] = {"!–- -–" ,"!DOCTYPE html" ,"a" ,"address" ,"area" ,"article" ,"aside" ,"audio" ,"b" ,"base" ,"bdi" ,"bdo" ,"blockquote" ,"body" ,"button" ,"canvas" ,"caption" ,"cite" ,"code" ,"col" ,"colgroup" ,"data" ,"datalist" ,"dd" ,"del" ,"details" ,"dfn" ,"dialog" ,"div" ,"dl" ,"dt" ,"em" ,"embed" ,"fieldset" ,"figure" ,"footer" ,"form" ,"h1" ,"h2" ,"h3" ,"h4" ,"h5" ,"h6" ,"head" ,"header" ,"hgroup" ,"hr" ,"html" ,"i" ,"iframe" ,"input" ,"ins" ,"keygen" ,"label" ,"legend" ,"li" ,"link" ,"main" ,"map" ,"mark" ,"menu" ,"menuitem" ,"meta" ,"meter" ,"nav" ,"noscript" ,"object" ,"ol" ,"optgroup" ,"option" ,"output" ,"p" ,"param" ,"pre" ,"progress" ,"q" ,"rb" ,"rp" ,"rt" ,"rtc" ,"ruby" ,"s" ,"script" ,"section" ,"select" ,"small" ,"source" ,"span" ,"strong" ,"style" ,"sub" ,"summary" ,"sup" ,"table" ,"tbody" ,"td" ,"template" ,"textarea" ,"tfoot" ,"th" ,"thead" ,"time" ,"title" ,"tr" ,"track" ,"u" ,"ul" ,"var" ,"video" ,"wbr"}; 
    for(int i = 0; i < 110; i ++){ //iterate len of html array
        if(strcmp(*(htmlelements + i), elem) == 0){ 
            res = 1; 
            break; 
        }
    }
    return res; 
}

void printtree(htmlnode *root){
    if(!root){ 
        printf("empty tree!");
        return; 
    }

    int isHtmlElement = isElement(root -> val); // 1 = true, 0 = false 
    if(isHtmlElement == 1){ 
        printf("<%s>", root -> val);  
    }
    else{ 
        printf("%s", root -> val);  
    }
    
    if(root -> left){ 
        printtree(root -> left); 
    }

    if(root -> right){ 
        printtree(root -> right); 
    }

    if(isHtmlElement == 1){ 
        printf("<%s>", root -> val);  
    }
}


// int main(){
//     htmlnode *node1 = buildnode("div", buildnode("h1", buildnode("hello world!", NULL, NULL), NULL), NULL ); 
//     printtree(node1); 
//     return 0; 
// }


