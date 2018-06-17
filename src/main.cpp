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

	// twitter
	CTML::Node twitter_script = CTML::Node("script");
	twitter_script.SetAttribute("src", "https://platform.twitter.com/widgets.js");
	twitter_script.SetAttribute("charset", "utf-8");
	doc.AddNodeToBody(twitter_script);
	CTML::Node twitter = CTML::Node("a");
	twitter.SetAttribute("class", "twitter-timeline");
	twitter.SetAttribute("href", "https://twitter.com/nevromeCS?ref_src=twsrc%5Etfw");
	twitter.SetAttribute("data-height", "390px");

	// github
	CTML::Node github = CTML::Node("iframe");
	github.SetAttribute("class", "iframe");
	github.SetAttribute("allowtransparency", "true");
	github.SetAttribute("frameborder", "0");
	github.SetAttribute("scrolling", "yes");
	github.SetAttribute("seamless", "seamless");
	github.SetAttribute("src", "http://colmdoyle.github.io/gh-activity/gh-activity.html?user=nevrome&type=user");
	github.SetAttribute("width", "100%");
	github.SetAttribute("height", "390px");

	// hcommons
	CTML::Node hcommons = CTML::Node("iframe");
	hcommons.SetAttribute("class", "iframe");
	hcommons.SetAttribute("allowtransparency", "true");
	hcommons.SetAttribute("frameborder", "0");
	hcommons.SetAttribute("scrolling", "yes");
	hcommons.SetAttribute("seamless", "seamless");
	hcommons.SetAttribute("src", "https://hcommons.org/members/nevrome/");
	hcommons.SetAttribute("width", "100%");
	hcommons.SetAttribute("height", "390px");

	// github 2
	CTML::Node github_repos_css = CTML::Node("link");
	github_repos_css.SetAttribute("href", "github-widget/github-widget.css");
	github_repos_css.SetAttribute("type", "text/css");
	github_repos_css.SetAttribute("rel", "stylesheet");	
	doc.AddNodeToHead(github_repos_css);
	CTML::Node github_repos_script = CTML::Node("script");
	github_repos_script.SetAttribute("src", "github-widget/github-widget.js");
	github_repos_script.SetAttribute("charset", "utf-8");
	CTML::Node github_repos_nevrome = CTML::Node("div");
	github_repos_nevrome.SetAttribute("class", "github-widget");
	github_repos_nevrome.SetAttribute("data-user", "nevrome");
	CTML::Node github_repos_ISAAKiel = CTML::Node("div");
	github_repos_ISAAKiel.SetAttribute("class", "github-widget");
	github_repos_ISAAKiel.SetAttribute("data-user", "ISAAKiel");

	// header archaeologist
	CTML::Node archaeologist = CTML::Node("div");
	archaeologist.SetAttribute("class", "top_boxes");
	archaeologist.SetAttribute("style", "background-color: #006650;");
	
	CTML::Node hcommons_header = CTML::Node("h2");
	hcommons_header.SetAttribute("style", "color:white;");
	hcommons_header.SetContent("Archaeology");

	archaeologist.AppendChild(hcommons_header);

	// header developer

	CTML::Node developer = CTML::Node("div");
	developer.SetAttribute("class", "top_boxes");
	developer.SetAttribute("style", "background-color: #4B88A2;");

	CTML::Node github_header = CTML::Node("h2");
	github_header.SetAttribute("style", "color:white;");
	github_header.SetContent("Software projects");
	
	developer.AppendChild(github_header);
	
	//CTML::Node project_table = CTML::Node("table");
	//project_table.SetAttribute("style", "width:100%");
//	CTML::Node ptr1 = CTML::Node("tr");
	//CTML::Node pth1 = CTML::Node("th");
//	pth1.SetContent("test");

	//ptr1.AppendChild(pth1);		
	//project_table.AppendChild(ptr1);
	//developer.AppendChild(project_table);

	// header beyond

	CTML::Node beyond = CTML::Node("div");
	beyond.SetAttribute("class", "top_boxes");
	beyond.SetAttribute("style", "background-color: #1da1f2;");	

	CTML::Node twitter_header = CTML::Node("h2");
	twitter_header.SetAttribute("style", "color:white;");
	twitter_header.SetContent("Contact");

	beyond.AppendChild(twitter_header);

	// actual page structure
	
	// row1	

	CTML::Node row = CTML::Node("div").SetAttribute("class", "row"); 

	CTML::Node column_hcommons = CTML::Node("div").SetAttribute("class", "column"); 
	CTML::Node column_twitter = CTML::Node("div").SetAttribute("class", "column"); 
	CTML::Node column_github = CTML::Node("div").SetAttribute("class", "column"); 
	
	column_hcommons.AppendChild(archaeologist);
	column_hcommons.AppendChild(hcommons);
	column_github.AppendChild(developer);
	column_github.AppendChild(github);
	column_twitter.AppendChild(beyond);
	column_twitter.AppendChild(twitter);

	row.AppendChild(column_hcommons);	
	row.AppendChild(column_github);	
	row.AppendChild(column_twitter);	

	// row2

	CTML::Node row2 = CTML::Node("div").SetAttribute("class", "row"); 

	CTML::Node column_github_nevrome = CTML::Node("div").SetAttribute("class", "column2"); 
	CTML::Node column_github_ISAAKiel = CTML::Node("div").SetAttribute("class", "column3"); 

	column_github_nevrome.AppendChild(github_repos_nevrome);	
	column_github_ISAAKiel.AppendChild(github_repos_ISAAKiel);	
	
	row2.AppendChild(column_github_nevrome);
	row2.AppendChild(column_github_ISAAKiel);

	doc.AddNodeToBody(row);
	doc.AddNodeToBody(row2);		

	doc.AddNodeToBody(github_repos_script);

	std::string index_html_path = ideas_file_path + "index.html";
  return doc.WriteToFile(index_html_path, CTML::Readability::MULTILINE);
}

