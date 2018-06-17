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
	twitter.SetAttribute("data-height", "510px");

	// github
	CTML::Node github = CTML::Node("iframe");
	github.SetAttribute("class", "iframe");
	github.SetAttribute("allowtransparency", "true");
	github.SetAttribute("frameborder", "0");
	github.SetAttribute("scrolling", "yes");
	github.SetAttribute("seamless", "seamless");
	github.SetAttribute("src", "https://cdoyle.me/gh-activity/gh-activity.html?user=nevrome&type=user");
	github.SetAttribute("width", "100%");
	github.SetAttribute("height", "510px");
	github.SetAttribute("sandbox", "allow-forms allow-scripts");

	// hcommons
	CTML::Node hcommons = CTML::Node("iframe");
	hcommons.SetAttribute("class", "iframe");
	hcommons.SetAttribute("allowtransparency", "true");
	hcommons.SetAttribute("frameborder", "0");
	hcommons.SetAttribute("scrolling", "yes");
	hcommons.SetAttribute("seamless", "seamless");
	hcommons.SetAttribute("src", "https://hcommons.org/members/nevrome/");
	hcommons.SetAttribute("width", "100%");
	hcommons.SetAttribute("height", "510px");

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

	// top box archaeology
	CTML::Node archaeology = CTML::Node("div");
	archaeology.SetAttribute("class", "top_boxes");
	archaeology.SetAttribute("style", "background-color: #006650;");
	
	CTML::Node archaeology_text = CTML::Node("p");
	archaeology_text.SetContent(
		" I'm <b>Clemens Schmid</b>, a student of Pre- and Protohistoric Archaeology \
			with a focus on Computational Archaeology. This is my personal website. <br> \
			I try to keep track of my academic career, talks and papers in my \
			<a href=\"https://hcommons.org/members/nevrome/\">Humanities Commons</a> profile. "
	);

	archaeology.AppendChild(archaeology_text);

	// header developer

	CTML::Node developer = CTML::Node("div");
	developer.SetAttribute("class", "top_boxes");
	developer.SetAttribute("style", "background-color: #4B88A2;");

	CTML::Node developer_text = CTML::Node("p");
	developer_text.SetContent(
		" I'm working on some scientific open source software development projects - \
			mostly in R and C++. <br> \
			All of them are on github. Some on \
			<a href=\"https://github.com/nevrome\">my own profile</a>, some \
			on the organisational profile of the working group \
			<a href=\"https://github.com/ISAAKiel\">ISAAK</a>. "
	);
	
	developer.AppendChild(developer_text);
	
	// header beyond

	CTML::Node contact = CTML::Node("div");
	contact.SetAttribute("class", "top_boxes");
	contact.SetAttribute("style", "background-color: #1da1f2;");	

	CTML::Node contact_text = CTML::Node("p");
	contact_text.SetContent(
		" The best way to contact me is via E-mail: <a href=\"&#x6d;&#x61;&#x69;&#x6c;&#x74;&#111;&#x3a;&#99;&#x6c;&#x65;&#109;&#101;&#x6e;&#x73;&#x40;&#x6e;&#101;&#x76;&#x72;&#x6f;&#109;&#101;&#46;&#x64;&#101;\">&#x63;&#x6c;&#101;&#109;&#101;&#x6e;&#x73;&#x40;&#x6e;&#101;&#118;&#x72;&#111;&#x6d;&#x65;&#46;&#x64;&#101;</a>. <br> \
		  I'm also on <a href=\"https://twitter.com/nevromecs\">twitter</a> and I love \
			testing out the latest messenger apps. I'm currently living in Kiel (Germany) if \
			you want to meet me in person. "
	);

	contact.AppendChild(contact_text);

	// actual page structure
	
	// row1	

	CTML::Node row = CTML::Node("div").SetAttribute("class", "row"); 

	CTML::Node column_hcommons = CTML::Node("div").SetAttribute("class", "columnL"); 
	CTML::Node column_github = CTML::Node("div").SetAttribute("class", "columnM"); 
	CTML::Node column_twitter = CTML::Node("div").SetAttribute("class", "columnR"); 
	
	column_hcommons.AppendChild(archaeology);
	column_hcommons.AppendChild(hcommons);
	column_github.AppendChild(developer);
	column_github.AppendChild(github);
	column_twitter.AppendChild(contact);
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

