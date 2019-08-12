---
title: "madop - A Markdown-to-HTML converter."
date: \today{}
author: "Pontus Ernstedt"
numbersections: true
bibliography: "bib/references.bib"
csl: "bib/ieee.csl"
link-citations: true
---

# Overview
A command-line interface tool to convert markup from one common markup language
to another, specifically Markdown to HTML, has been developed. The tool aims to
be simple to use and to its best intent handle the syntactical rules mentioned
in the original Markdown specification. The implementation is split into two
modules - a Markdown parser and an HTML renderer - both written in Haskell. A
tree-like data structure able to capture the structure of a formatted document
has been defined, that works as an intermediator between the two modules as
instances of it is ought to be produced by the parser and sent as input to the
renderer. 

# What is Markdown?
Markdown refers partly to a specification of a markup language, with rules on
how it should be translated into HTML, along with a tool - `Markdown.pl` - that
implements this specification. It was written by John Gruber in 2004 and both
the specification and the tool are available through his website [@mdspec].
Markdown has since seen a massive rise in usage, and multiple deviations of
the language have been developed. For example, GitHub offers a slightly
modified version of Markdown for users to format documentation with [@gfmspec].

Markdown features a set of pretty straightforward syntactical rules. A select
set of commonly used element constructs are supported, whereas any constructs
not supported can be created by writing raw HTML. The objective of Markdown is
to be as easy-to-read and easy-to-write as is feasible, often leaving the markup
readable even before converted, as opposed to many other markup languages such
as LaTeX.

Despite its popularity, Markdown has been the issuer of a lot of controversy
[@mdcontroversy1; @mdcontroversy2; @mdcontroversy3].  This is mainly due to the
fact that the original specification holds ambiguity and is not as
thorough as one might have wanted, leaving many cases to be interpreted -
leading to the many deviations of Markdown we see today.  While John Gruber
himself has disregarded this [@grubertweet], an attempt to establish a language
specification that further clarifies syntax rules and conforms to many common
deviations has been proposed, which goes under the name CommonMark. 

The tool discussed in this report attempts to accost to the syntactic rules
mentioned in the original specification only. Therefore, the term 'Markdown'
refers to the original specification as published by John Gruber unless
explicitly stated. In many ways, it is an attempt to reimplement
`Markdown.pl`. Where personal interpretation of the specification has been
needed, often the behavior of `Markdown.pl` has been mimicked. Any known
deviations will be mentioned throughout the text. This report assumes basic
knowledge of some common formatting rules in Markdown.

An important thing to note is that there is no such thing as invalid syntax
in Markdown. If a user misuses some formatting rule it only means that the
intended formatting will not take place and that the text will be interpreted
as is or possibly as some other, unintended rule instead. So while
HTML generated from Markdown might not match a users expectation, the
converter should never fail and display a syntax-error message.

# What is madop?  
madop can be described with the one-liner presented in the title - it's a
conversion tool that takes Markdown-formatted text and generates HTML markup
corresponding to the text. madop operates by first parsing the Markdown (coming
either from a file or as user input) into an intermediate tree-like data
structure that captures the structural elements of the document and their
content. This structure is then put into a renderer that traverses the
structure and generates HTML for every node. Finally, all generated lines are
put together to create the final HTML-document for the user.

## Intermediate structure 
If you are familiar with HTML, it should be clear that an HTML-document,
besides metadata, can be viewed as a series of block and inline elements.
Examples of block elements are paragraphs, headers, and horizontal rules. Inline
elements often reside inside block elements and, along with plain text, make
out the content of the block.  Examples are emphasized text, images, and
line breaks.

A natural way of representing an HTML-document is using the Document Object
Model (DOM). The DOM treats the document as having a logical structure, seen
as that of a tree [@dom], wherein each node represents an element and a subtree
represents an element with its child nodes being its content. While it's worth
mentioning that the DOM also specifies an API through which one can access and
manipulate nodes, for the sake of this report we're only interested in the
theoretical structural representation of a document.

As mentioned, madop uses an intermediate data structure that a document is
parsed into and rendered from. This structure, internally named `Document`
largely resembles the tree structure used in the DOM. Looking at it from a
top-down perspective, the outermost document node is implicit and is really
just a list of _blocks_. In Markdown, inline elements can never occur outside
of block elements, which is why a list of blocks is sufficient. For a document
to then be representable using the `Document` type, the data type used to
represent a block must cover all the block-types that Markdown supports.
Depending on the type, a block can stand on its own, include data relevant to
the block, contain inline elements, recursively contain other blocks, or a
combination of the above. To clarify, let's look at a few examples. The block
representing a horizontal rule needs no additional data nor other elements, and
therefore stands on its own. A header holds data indicating what header level
it represents (as to specify whether it should be rendered into an `h1` or an
`h3`, for example), and also holds multiple inline elements forming the content
of the header. The block quote is an example of a block that consists of
multiple other blocks (technically, another `Document`).

madop uses an algebraic data type (ADT) to represent either of the block-types,
internally known as `Block`. Due to the varying characteristics of blocks
identified above, an ADT fits well for this situation. It can be instanced
into either of the block-types represented in Markdown, with each constructor
demanding the data (if any) needed for the block it represents. This also means
that the constructors that require other `Block`-instances make the `Block`
type recursively-defined.

The design of the `Block` data type is also what enables us to form a tree
able to represent the structure of the document. As mentioned, the implicit
root node `Document` was just a sequence of blocks. The `Block` data
type therefore also implicitly represents a node in the tree. The `Block`
instances that contain other elements thereby form the root of a subtree 
representing said block and its content. Given that every block is seen
as a tree, a `Document` is technically not a tree but a forest, although
making this distinction is not necessary. Figure 1 illustrates what
a simple document might look like.

![An example tree capturing the structure of a simple document.](img/tree.pdf)

Inline elements are represented similarly to blocks, but as a separate
algebraic data type - `Inline`. As with blocks, the `Inline` constructors take
different data depending on the type of element. Most inline elements hold only
primitive data - raw strings - meaning they often represent the leaves of a
document tree. However, some elements such as emphasize hold other inline
elements, again defined recursively, resulting in non-leaf inline elements. It
should be noted that the `Inline` type is also used to represent some objects
that are not actual elements. Such is the case with `Text`, for example,
representing plain text.

One could imagine having an `Element` type that represents either a `Block` or
an `Inline` to capture their shared characteristics. However, there has been no
identified use case for this, as the element-types are always used separately,
meaning no such data type exists.

## Parser 
The core of madop lies in its parser module. The parsers job is to translate a
given Markdown-formatted string into the intermediate structural representation
of said string. In other words, it is ought to create an instance of `Document`
which captures the structure of the document. 

madops parser uses a parsing technique called *combinatory parsing*. The idea
is that the parser accepting input in the general case is built up using less
complex parsers - ones that only accept more specific input. To do this, one
uses *parser combinators* - higher-order functions that takes parsers as input
and produces a parser capable of parsing more complex patterns. To exemplify,
one can imagine a whitespace-parser that only succeeds if the next character in
an input stream is a whitespace character. This parser could then be sent as
argument to a combinator that takes said parser and applies it multiple times
until there is no more whitespace at the start of the stream.  This combinator,
commonly referred to as the many-combinator, has now produced a parser
accepting input in a more general case (*any* number of whitespace) compared to
that of the initial parser (accepting *exactly one* whitespace).

There are a few parser combinator libraries available for Haskell that
provide a set of commonly used combinators. The one used in madop is called
Parsec. Parsec provides not only combinators, but also some primitive parsers
such as the whitespace-parser described above. Furthermore, it provides a
powerful data type that encapsulates internal parser state such as the internal
state of the input stream and provides a neat interface towards this using
common Haskell schemes. This not only makes it easier to write up own parsers,
but also enables them to be more readable and easier to reason about.

madops parser starts from the beginning of the input stream and works its way
to the end, identifying syntactical objects and their content. The parser works
in a try-and-rewind matter. At any given point, it attempts to apply a parser
and, if a syntactical error is hit, it rewinds (or backtracks) itself back to
where it started and tries applying another parser instead. For example, when
the parser expects a block element, it will attempt to apply the block-parsers
one at a time until one of them succeeds. 

The behavior described above is enabled through two commonly used combinators.
The try-combinator is what allows the parser to backtrack. When supplying a
parser to the try-combinator, it returns a parser with the same behavior except
that it makes sure to backtrack the parser state to where it was beforehand,
should it fail. Along with this, the choice-combinator gives us the ability to
supply multiple (block-)parsers from which it will run them one by one until one
of the parsers succeeds.

### Element parsers
In the parser module, there are separate parsing functions for each of the
different elements that Markdown supports. Given that each element has 
different syntactical rules, the implementation for each parser naturally
varies. To get an idea of how they may work, let's look at a few examples.

The most basic parser is the parser for plain text. This is the parser that is
applied when no other (inline-)parser accepted the input, and it will consume
characters until a character of special meaning (i.e. an opening symbol for one
of the other syntax rules) occurs.

The syntax for inline elements is pretty straightforward. They usually require
some special symbol(s) indicating the start of the element, then comes some
user-specified text followed by some closing symbol(s). Sequencing parsers
requiring certain symbols with arbitrary character-parsers is equally
straightforward. The biggest difficulty arises when trying to limit the inline
parsers' scope. If a parser successfully parses its special opening symbol, it
will attempt to parse user data until the closing symbol is hit. However,
Markdown requires inline elements to not span across blocks, which means the
parser must only attempt to parse characters until the end of the block. If the
closing symbol is not found, the parser must not accept the input, but instead
backtrack. 

Headers and paragraphs are the two block-types that hold multiple inline
elements as their content. A headers content simply ends when a newline (a `\n`
character) occurs. This is not the case when inside a paragraph however, as
paragraphs can span multiple lines. To tackle this, madops parser carries an
indicator in its internal state, indicating whether or not the parser is
currently inside a paragraph-block.  When an inline-parser hits a newline, it
therefore checks this indicator to see if it should continue parsing on the
next line or backtrack if it is not yet finished.

Block-parsers vary a bit more in their implementation. The header-parser either
requires specific opening or closing symbols (Markdown supports two styles of
headers) but other than that simply parses one or more inline elements until
its line ends, as mentioned above. While a paragraph requires no special
symbols, it does require special care to be taken when determining where it
ends. A paragraph ends if it is separated by a blank line to the following
block, *or* if it is directly followed by another block. This is why, when
parsing inlines whilst inside a paragraph, either of the ending criteria will
be checked for when a newline occurs, where as if it is not, the paragraph
content continues on the following line.

The parser that accepts block quotes requires a different parsing technique
than the other parsers, as it holds other blocks as its content. Thankfully,
the syntax for block quotes indicate exactly which lines should be quoted.
Therefore, the parser first identifies which lines belong to the block and
strips these of the opening block quote syntax (">\ "). It then sets the
input stream to the block of text it consumed, from which it parses out
a sequence of blocks - technically, another document.

### Referenced links and images 
Markdown supports what it calls referenced links and referenced images. The
idea is that prose should not be cluttered by long links and image sources.
Instead, Markdown allows the user to define links and images anywhere in the
document and bind the definition to an id, which can be used to issue the link
or image. Referenced links and images demands special care by the parser.
Firstly, reference definitions should never be included in the structural
representation of the document. That is, they should never be parsed as
elements. Instead, they need to be consumed from the document, whereas the id
must be mapped to the corresponding link or image source and optional title so
that the correct link or image data can be found when the id is used in a
reference.

To achieve this, madops parser does a separate pass on the input stream before
parsing of elements take place. In this pass, it identifies any lines containing
reference definitions, and *consumes* them. What this means is that the line is
removed from the input stream, and the consumed id is mapped to the reference data 
as it is stored in a hash map. This hash map is included in the parser state, so
that it is accessible when the actual parsing takes place. When the actual
parsing occurs, any time a link or image reference is struck, the parser can
run a lookup on the id to fetch the correct data.

### Handling every case
As mentioned, there is no such thing as an invalid Markdown document. The
implication of this is that when attempting to parse a block (although the same
idea applies when selecting an inline element) in the choice manner described
above it is critical that one of the parsers handed to the choice-combinator
succeeds. When it comes to parsing blocks, the paragraph-parser is ought to act
as a catch-all succeeding when none of the other block-parsers do. Actually,
the paragraph-parser would accept input that one of the other parsers would
accept as well. This means that the order of which parsers are tried matters,
and - for input accepted by multiple parsers - a priority between the different
parsers must be (and has been) established.

## Rendering
As mentioned, the renderer modules job is to take the structural tree produced by
the parser and generate HTML from it. This is arguably the easier step of the
markup-conversion, mainly due to how the intermediate structure was set up.

For every element that can be represented as a node in the structural
tree, the renderer has rules for how HTML should be rendered from it. The easiest
case deals with leaf elements which can be directly translated into their
corresponding HTML. A text node, for instance, can be rendered simply by
extracting its textual content. Perhaps even easier, a horizontal rule
contains no additional data, as it will always render into an `<hr />`-tag.

Some complexity is added when dealing with elements that themselves contain
other elements. For example, to render a header, the renderer first checks what
level the header should be of - this data is included in the node - to be able
to choose the correct header tag to wrap around the header content. The content,
however, is not yet a string but a sequence of inline elements - more nodes.
Therefore, the renderer first needs to render the HTML corresponding to these
child nodes (and possibly their child nodes too), before wrapping it all up in
the header tag decided earlier. Figure 2 shows an illustration of this.

![An overview of how HTML is rendered for a header by first rendering HTML
for its inner elements.](img/render.pdf)

It quickly becomes apparent that as long as the renderer knows how to deal with
a single node, to render HTML for the whole document all it needs to do is to
perform what can be compared to a pre-order traversal on the structural tree.
Since the document was represented as a sequence of blocks, the renderer
iterates over this sequence, rendering HTML-strings for every block (and their
subtrees) individually and concatenates all rendered strings into the final
HTML-document.

The implementation of all this is pretty straightforward. The renderer module
consists of two core rendering functions - one that renders a block element,
and one that renders an inline element. Using pattern matching on element
constructors, the specific element type can be determined and HTML rendered for
it accordingly.

It is worth noting that the rendering technique is not in any way bound to the
fact that it is ought to produce HTML. Quite the opposite actually, as the 
tree traversal-technique should be just as relevant no matter the output
markup. With this in mind, it should be clear to see how painlessly madop could 
be extended with renderers for other markups, by adding another renderer module
working on the intermediate structure and an option to use this renderer
when running madop.

## Usage 
Similar to the original `Markdown.pl`, madop is shipped as a command-line
interface (CLI) tool. Following the idea of many CLI tools, madop aims to do
one thing and do it well. As a result, this also makes madop easy to use. In
the simplest case, the user has written some Markdown in a file and wants to
have it converted into HTML. This is madops default case, and can be done
simply by typing `madop my-file.md` into the terminal, where `my-file.md` is
the relative file path of the file to be converted. madop will then answer by
dumping out the generated HTML to the terminal (well, `stdout`). While terminal
output might not always be preferred, this makes madop easy to use in
pipeline constructs. 

madop also supports a few option flags to alter the default use case and cover
some alternative execution paths. For example, by typing `madop --stdin` one
can have madop expect Markdown from standard input instead of reading it from a
file. This can be used if a user wants to quickly convert smaller Markdown
documents by inputting them directly into the terminal. More importantly, this
allows madop to run on text generated by another program in a pipeline.
Furthermore, the user can supply a file to which the output is ought to be
written using the output flag as such: `madop --output my-file.html
my-file.md`.

madop supports the formatting rules found in the specification. As of yet,
however, support for lists has not been implemented. If lists are needed, a
workaround, though not as smooth as using Markdown syntax, would be to write
them up in raw HTML.

## Quality Assurance 
As with any software, thorough testing is important and should be part of the
development repository from the beginning. Since there are infinitely many ways
one could write up a Markdown document, consistent identification and testing of
(edge) cases is critical to guarantee correct program behavior.

The testing strategy for madop has oriented around unit testing, along with 
regression testing. There are two test suites testing the parser and renderer
respectively. The test suite for the parser features test cases for each element
parser, along with cases testing behavior when multiple elements intertwine.
Regression testing is featured in the sense that cases have been added upon discovery
of (and fixing of) bugs, which has happened throughout the development.

To a smaller extent, manual testing has been deployed, comparing program output
between madop and Markdown.pl by running both programs and piping output to the
terminal tool `diff`. This was never automated however. The same goes for manual
integration testing.

madop is considered stable when used on rational documents.  Rest assured,
there will be missed cases, especially regarding abusive documents. Abusive
documents refer here to documents that intentionally try to use Markdowns
ambiguity to break the tool - this includes nonsensitive opening of formatting
rules, not escaping special symbols, intertwining blocks in an unnatural
manner, etc. While this kind of behavior is allowed and should not make madop
crash, the rendered HTML might not be what was expected.  This is, however, a
case where Markdowns ambiguity plays in - a user might have different
expectations on the output than what was interpreted (or simply decided upon)
by the developer during implementation.

As it stands, no optimizations have been carried out regarding performance.
The parsing is known to be slow when run on large documents, especially abusive
documents. This is a direct effect of the arbitrary lookahead that madop (and
Markdown itself) requires.

# Personal reflection
All in all, this project has been a fun learning experience. It has been very
different compared to previous school projects, given how freely I was to
select both *what* to work on and *how* to work. Furthermore, it is my first
project carried out solo, as opposed to previous projects all carried out in
teams. While working in a group better represents the reality of a developer, I
feel like working solo has given me some valuable insights. I have a better
understanding of how I prefer to structure and carry out the workload
involved in a project, something I feel every member of a team needs to have
for a middle ground - one that acknowledges every members' preferences - to be
found.

Throughout this project, I have applied both theoretical and practical
knowledge from previous courses. Perhaps most obvious is my usage of the
language Haskell, which was the language that was used in my introductory
algorithms and data structures course. While the follow up programming
paradigms course introduced imperative and object oriented ideas and languages,
patterns from that course has of course been applicable as
well. This follow up course also gained me some practical proficiency in
writing parsers, all the while reading the more theoretical automata theory
alongside which introduced the concept of grammars. 

I feel like my biggest triumphs in this project lie outside of the product
itself.  While I am happy with what was produced, madop that is, I take more
pride when looking back at how I have worked. I feel like I successfully scoped
the project as to what was reasonable for each week, and that I delivered what
was planned more often than not. The goal was never to add as many features as
possible, but to continually deliver a complete (albeit featureless) product
for each week. Furthermore, I feel like my usage of tools (some completely new
to me) has been gratifying, and that my workflow has improved greatly because
of this. The project (git-)repository is well structured and offers a good
timeline of how madop was built.

My goal with this project was never to make something new or groundbreaking -
there are multiple, better Markdown-converters out there. Instead, I wanted to
learn more about concepts related to the product, and gain an insight into how
these other tools might work. I do acknowledge that I might not have gotten a
theoretical understanding of concepts such as parsing or grammars as thorough
as I might have wanted. This is to be expected though, as this was never a
theoretical course. I instead recognize that having done this project will help
me when these concepts show up in the future. Furthermore, I feel like I have
an insight into how a very sophisticated tool like Pandoc (*the* tool for
converting markup) is implemented, having studied it throughout the development
of madop.

\newpage
# References
