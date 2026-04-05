---
permalink: /
title: "Git supported webpage"
author_profile: true
redirect_from: 
  - /about/
  - /about.html
---

In this tab, we present several _features_ that are supported by Jekyll. The main configuration file for the site is in the base directory in _config.yml, which defines the content in the sidebars and other site-wide features. The configuration file for the top menu is in _data/navigation.yml.

Features
====
This is how to [Link a Page](https://www.math.cit.tum.de/math/forschung/gruppen/statistics/people/). 

Or how to separate paragraphs and add a header.
======
See Above. 

New Paragraph.

Listed Items:
======
1. First point. 
1. Same level of indentation. 
	1. Subitem
	
* This is an unordered item


Create content & metadata
------
For site content, there is one markdown file for each type of content, which are stored in directories like _publications, _talks, _posts, _teaching, or _pages. For example, each talk is a markdown file in the [_talks directory](https://github.com/academicpages/academicpages.github.io/tree/master/_talks). At the top of each markdown file is structured data in YAML about the talk, which the theme will parse to do lots of cool stuff. The same structured data about a talk is used to generate the list of talks on the [Talks page](https://academicpages.github.io/talks), each [individual page](https://academicpages.github.io/talks/2012-03-01-talk-1) for specific talks, the talks section for the [CV page](https://academicpages.github.io/cv), and the [map of places you've given a talk](https://academicpages.github.io/talkmap.html) (if you run this [python file](https://github.com/academicpages/academicpages.github.io/blob/master/talkmap.py) or [Jupyter notebook](https://github.com/academicpages/academicpages.github.io/blob/master/talkmap.ipynb), which creates the HTML for the map based on the contents of the _talks directory).

**Markdown generator**

The repository includes [a set of Jupyter notebooks](https://github.com/academicpages/academicpages.github.io/tree/master/markdown_generator
) that converts a CSV containing structured data about talks or presentations into individual markdown files that will be properly formatted for the Academic Pages template. The sample CSVs in that directory are the ones I used to create my own personal website at stuartgeiger.com. My usual workflow is that I keep a spreadsheet of my publications and talks, then run the code in these notebooks to generate the markdown files, then commit and push them to the GitHub repository.