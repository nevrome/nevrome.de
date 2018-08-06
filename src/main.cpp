#include "CTML.h"

int main(int argc, char* argv[]){

	// read html output path from input arguments
	if (argc < 2) { return(EXIT_FAILURE); }
 	std::string ideas_file_path = argv[1];

	// create general page object
	CTML::Document doc;
	
	// load css
	CTML::Node ref_to_css = CTML::Node("link");
	ref_to_css.SetAttribute("href", "styles.css");
	ref_to_css.SetAttribute("type", "text/css");
	ref_to_css.SetAttribute("rel", "stylesheet");	
	doc.AddNodeToHead(ref_to_css);

	//// prepare iframes and widgets ////

	// prepare hcommons iframe 
	CTML::Node hcommons = CTML::Node("iframe");
	hcommons.SetAttribute("class", "iframe");
	hcommons.SetAttribute("allowtransparency", "true");
	hcommons.SetAttribute("frameborder", "0");
	hcommons.SetAttribute("scrolling", "yes");
	hcommons.SetAttribute("seamless", "seamless");
	hcommons.SetAttribute("src", "https://hcommons.org/members/nevrome/");
	hcommons.SetAttribute("width", "100%");
	hcommons.SetAttribute("height", "900px");

	// prepare github activity iframe
	CTML::Node github = CTML::Node("iframe");
	github.SetAttribute("class", "iframe");
	github.SetAttribute("allowtransparency", "true");
	github.SetAttribute("frameborder", "0");
	github.SetAttribute("scrolling", "yes");
	github.SetAttribute("seamless", "seamless");
	github.SetAttribute("src", "https://cdoyle.me/gh-activity/gh-activity.html?user=nevrome&type=user");
	github.SetAttribute("width", "100%");
	github.SetAttribute("height", "900px");
	github.SetAttribute("sandbox", "allow-forms allow-scripts");

	// prepare twitter activity widget
	CTML::Node twitter_script = CTML::Node("script");
	twitter_script.SetAttribute("src", "https://platform.twitter.com/widgets.js");
	twitter_script.SetAttribute("charset", "utf-8");
	doc.AddNodeToBody(twitter_script);
	CTML::Node twitter = CTML::Node("a");
	twitter.SetAttribute("class", "twitter-timeline");
	twitter.SetAttribute("href", "https://twitter.com/nevromeCS?ref_src=twsrc%5Etfw");
	twitter.SetAttribute("data-height", "900px");

	//// prepare text boxes on the left ////

	// text box 1
	CTML::Node archaeology = CTML::Node("div");
	archaeology.SetAttribute("class", "text_boxes");
	archaeology.SetAttribute("style", "background-color: #672a4e;");
	
	CTML::Node archaeology_text = CTML::Node("p");
	archaeology_text.SetContent(
		" I'm <b>Clemens Schmid</b>, a student of Pre- and Protohistoric Archaeology \
			with a focus on Computational Archaeology. This is my personal website. <br> \
			I try to keep track of my academic career, talks and papers in my \
			<a href=\"https://hcommons.org/members/nevrome/\">Humanities Commons</a> profile. "
	);

	archaeology.AppendChild(archaeology_text);

	// text box 2
	CTML::Node developer = CTML::Node("div");
	developer.SetAttribute("class", "text_boxes");
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
	
	// text box 3
	CTML::Node contact = CTML::Node("div");
	contact.SetAttribute("class", "text_boxes");
	contact.SetAttribute("style", "background-color: #444054;");	

	CTML::Node contact_text = CTML::Node("p");
	contact_text.SetContent(
		" The best way to contact me is via E-mail: <a href=\"&#x6d;&#x61;&#x69;&#x6c;&#x74;&#111;&#x3a;&#99;&#x6c;&#x65;&#109;&#101;&#x6e;&#x73;&#x40;&#x6e;&#101;&#x76;&#x72;&#x6f;&#109;&#101;&#46;&#x64;&#101;\">&#x63;&#x6c;&#101;&#109;&#101;&#x6e;&#x73;&#x40;&#x6e;&#101;&#118;&#x72;&#111;&#x6d;&#x65;&#46;&#x64;&#101;</a>. <br> \
		  I'm also on <a href=\"https://twitter.com/nevromecs\">twitter</a> and I love \
			testing out the latest messenger apps. I'm currently living in Kiel (Germany) if \
			you want to meet me in person. "
	);

	contact.AppendChild(contact_text);

	//// construct avatar image ////

	CTML::Node avatar_box = CTML::Node("div");
	avatar_box.SetAttribute("class", "avatar_box");

	CTML::Node avatar = CTML::Node("img");
	avatar.SetAttribute("src", "avatar.jpg");
	avatar.SetAttribute("alt", "Avatar");
	avatar.SetAttribute("width", "100%");
	avatar.SetAttribute("style", "border-radius: 50%;");

	avatar_box.AppendChild(avatar);

	//// construct page from prepared elements ////
	
	// row 1	
	CTML::Node row = CTML::Node("div").SetAttribute("class", "row"); 
 
	CTML::Node column_clemens = CTML::Node("div").SetAttribute("class", "columnL");
	CTML::Node column_hcommons = CTML::Node("div").SetAttribute("class", "columnML"); 
	CTML::Node column_github = CTML::Node("div").SetAttribute("class", "columnMR"); 
	CTML::Node column_twitter = CTML::Node("div").SetAttribute("class", "columnR"); 
	
	column_clemens.AppendChild(avatar_box);
	column_clemens.AppendChild(archaeology);
	column_clemens.AppendChild(developer);
	column_clemens.AppendChild(contact);

	column_hcommons.AppendChild(hcommons);
	column_github.AppendChild(github);
	column_twitter.AppendChild(twitter);

	row.AppendChild(column_clemens);
	row.AppendChild(column_hcommons);	
	row.AppendChild(column_github);	
	row.AppendChild(column_twitter);	

	//// final construction and output ////

	doc.AddNodeToBody(row);

	std::string index_html_path = ideas_file_path + "index.html";
  return doc.WriteToFile(index_html_path, CTML::Readability::MULTILINE);
}

