#include "CTML.h"

int main(int argc, char* argv[]){

	if (argc < 2) { return(EXIT_FAILURE); }
 	std::string ideas_file_path = argv[1];

	CTML::Document doc;
	doc.AddNodeToBody(CTML::Node("a.link").SetContent("Anchor").SetAttribute("href", "http://www.example.com"));

	std::string index_html_path = ideas_file_path + "index.html";
  return doc.WriteToFile(index_html_path, CTML::Readability::MULTILINE);
}

