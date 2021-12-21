/*
 *  A function illustrating how to link C code to code generated from LLVM 
 */

#include <stdio.h>
#include <string.h>

void createHTMLDocument(char *cssFile){ 
  printf("<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"%s\"></head><body>", cssFile);
}

void createElement(char *elementName, char *className,  char *innerHtml){
  if( strcmp("img", elementName) == 0  || strcmp("input",elementName) == 0 ){
    printf("<%s class=\"%s\" src=\"%s\">\n", elementName, className, innerHtml);   
  }else{
    printf("<%s class=\"%s\">%s</%s>\n", elementName, className, innerHtml, elementName);
  }
}

#ifdef BUILD_TEST
int main()
{
  char s[] = "HELLO WORLD09AZ";
  char *c;
  for ( c = s ; *c ; c++) printbig(*c);
}
#endif
