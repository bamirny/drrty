#include <stdio.h>
#include <string.h>

/* HTML functions */ 
void createHTMLDocument(char *cssFile){ 
  printf("<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"%s\"></head><body>", cssFile);
}

void createElement(char *elementName, char *innerHtml, char *className){
  if( strcmp("img", elementName) == 0  || strcmp("input",elementName) == 0 ){
    printf("<%s class=\"%s\" src=\"%s\">\n", elementName, className, innerHtml); // <img class="" src=""> 
  }else{
    printf("<%s class=\"%s\">%s</%s>\n", elementName, className, innerHtml, elementName);
  }
}

void createHTML(char *html){
  printf("%s", html);
}

void makeHeader(char *innerHtml){ 
  printf("<h1>%s</h1>\n", innerHtml);
}

void makeText(char *innerHtml){ 
  printf("<p>%s</p>\n", innerHtml);
}

void makeImage(char *innerHtml){ 
  printf("<img src=\"%s\">\n", innerHtml); 
}

void makeInput(char *className){ 
  printf("<input class=\"%s\">\n", className); 
}

#ifdef BUILD_TEST
int main()
{
  char s[] = "HELLO WORLD09AZ";
  char *c;
  for ( c = s ; *c ; c++) printbig(*c);
}
#endif
