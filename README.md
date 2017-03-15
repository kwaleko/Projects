# Projects
## Problem

Create a Text area so the blog user can use it and post any given post using some code as per the below
- /* /* to make the text between the code as Title
- {- {- use this code to highlight the text in yellow
- Other (bold, italic, underline ...)

the language used to build the Post page is [ELM](http://elm-lang.org), the Html in ELM language is function, so we need to parse the text/post entered by the blog user and transfor it to ELM-HTML.
for exmaple :
- if the user enter /* bold {- Bold and higlight {- /* it should be parsed to : div [] [B[][ text "Bold" , mark[] [text "Bold and higlight"]]]

using a regular expression to achieve the above very limited and overcomple in such case

## Solution
the solution was to use [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator)
for this given problem, I have used [combine library](http://package.elm-lang.org/packages/Bogdanp/elm-combine/latest/Combine)

##### Note : the parse is available on addPost.elm page starting from line 166
the below example show how the parser works
## Working Example
![alt tag](https://i.stack.imgur.com/vSx2Y.png)

