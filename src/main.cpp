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

	// reddit 
	CTML::Node reddit = CTML::Node("script");
	reddit.SetAttribute("type", "text/javascript");
	reddit.SetAttribute("src", "https://www.reddit.com/user/nevrome/submitted.embed?limit=30&sort=hot");
	reddit.SetContent("huhu");

	// actual page structure
	
	CTML::Node left = CTML::Node("div").SetAttribute("class", "left"); 
	left.AppendChild(twitter);
	CTML::Node middle = CTML::Node("div").SetAttribute("class", "middle"); 
	middle.AppendChild(github);
	CTML::Node right = CTML::Node("div").SetAttribute("class", "right"); 
	right.AppendChild(reddit);

	doc.AddNodeToBody(left);
	doc.AddNodeToBody(middle);
	doc.AddNodeToBody(right);

	std::string index_html_path = ideas_file_path + "index.html";
  return doc.WriteToFile(index_html_path, CTML::Readability::MULTILINE);
}

