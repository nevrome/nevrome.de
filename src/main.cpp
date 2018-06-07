#include "CTML.h"

int main(int argc, char* argv[]){

	if (argc < 2) { return(EXIT_FAILURE); }
 	std::string ideas_file_path = argv[1];

	CTML::Document doc;
	
	// css
	CTML::Node ref_to_css = CTML::Node("link");
	ref_to_css.SetAttribute("href", "styles.css");
	ref_to_css.SetAttribute("type", "text/css");
	ref_to_css.SetAttribute("rel", "stylesheet");	
	doc.AddNodeToHead(ref_to_css);

	doc.AddNodeToBody(CTML::Node("a.link").SetContent("Anchor").SetAttribute("href", "http://www.example.com"));
	
	CTML::Node my_button = CTML::Node("button");
	my_button.SetAttribute("style", "vertical-align:middle");
	my_button.SetAttribute("class", "button");
	my_button.AppendChild(CTML::Node("span").SetContent("Hover"));	
	doc.AddNodeToBody(my_button);
	
	
		

	std::string index_html_path = ideas_file_path + "index.html";
  return doc.WriteToFile(index_html_path, CTML::Readability::MULTILINE);
}

