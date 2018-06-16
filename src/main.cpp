#include "CTML.h"

int main(int argc, char* argv[]){

	if (argc < 2) { return(EXIT_FAILURE); }
 	std::string ideas_file_path = argv[1];

	//// load and prepare stuff ////

	CTML::Document doc;
	
	// css
	CTML::Node ref_to_css = CTML::Node("link");
	ref_to_css.SetAttribute("href", "styles.css");
	ref_to_css.SetAttribute("type", "text/css");
	ref_to_css.SetAttribute("rel", "stylesheet");	
	doc.AddNodeToHead(ref_to_css);

	// button
	CTML::Node my_button = CTML::Node("button");
	my_button.SetAttribute("style", "vertical-align:middle");
	my_button.SetAttribute("class", "button");
	my_button.AppendChild(CTML::Node("span").SetContent("Hover"));	
	
	// twitter
	CTML::Node twitter_script = CTML::Node("script");
	twitter_script.SetAttribute("src", "https://platform.twitter.com/widgets.js");
	twitter_script.SetAttribute("charset", "utf-8");
	doc.AddNodeToBody(twitter_script);
	CTML::Node twitter = CTML::Node("a");
	twitter.SetAttribute("class", "twitter-timeline");
	twitter.SetAttribute("href", "https://twitter.com/nevromeCS?ref_src=twsrc%5Etfw");
	twitter.SetAttribute("data-height", "100vh");

	// github
	CTML::Node github = CTML::Node("iframe");
	github.SetAttribute("allowtransparency", "true");
	github.SetAttribute("frameborder", "0");
	github.SetAttribute("scrolling", "yes");
	github.SetAttribute("seamless", "seamless");
	github.SetAttribute("src", "http://colmdoyle.github.io/gh-activity/gh-activity.html?user=nevrome&type=user");
	github.SetAttribute("width", "100%");
	github.SetAttribute("height", "100%");

	// hcommons
	CTML::Node hcommons = CTML::Node("iframe");
	hcommons.SetAttribute("allowtransparency", "true");
	hcommons.SetAttribute("frameborder", "0");
	hcommons.SetAttribute("scrolling", "yes");
	hcommons.SetAttribute("seamless", "seamless");
	hcommons.SetAttribute("src", "https://hcommons.org/members/nevrome/");
	hcommons.SetAttribute("width", "100%");
	hcommons.SetAttribute("height", "100%");

	// actual page structure
	
	CTML::Node row = CTML::Node("div").SetAttribute("class", "row"); 

	CTML::Node column1 = CTML::Node("div").SetAttribute("class", "column"); 
	CTML::Node column2 = CTML::Node("div").SetAttribute("class", "column"); 
	CTML::Node column3 = CTML::Node("div").SetAttribute("class", "column"); 
	
	column1.AppendChild(hcommons);
	column2.AppendChild(twitter);
	column3.AppendChild(github);

	row.AppendChild(column1);	
	row.AppendChild(column2);	
	row.AppendChild(column3);	

	doc.AddNodeToBody(row);	

	std::string index_html_path = ideas_file_path + "index.html";
  return doc.WriteToFile(index_html_path, CTML::Readability::MULTILINE);
}

