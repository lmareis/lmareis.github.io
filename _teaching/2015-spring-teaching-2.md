---
title: "Zenodo: Gallery and Search"
collection: teaching
type: "Munich"
permalink: /teaching/2015-spring-teaching-1
venue: "Germany"
date: 2025-01-13
location: "City, Country"
---
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Gallery with Links</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            text-align: left;
        }
        p {
            text-align: left; /* Change text alignment to left */
            max-width: 800px; /* Restrict paragraph width for readability */
            margin: 0 auto; /* Center the paragraph block */
        }
        .gallery-container {
            display: flex;
            flex-wrap: wrap;
            justify-content: center;
            gap: 40px; /* Spacing between gallery items */
            margin-top: 20px;
        }
        .gallery-item {
            text-align: center;
            max-width: 200px;
        }
        .gallery-item img {
            width: 100%;
            border: 1px solid #ccc;
            border-radius: 8px;
        }
        .button-container {
            display: flex;
            justify-content: center;
            gap: 10px; /* Space between buttons */
            margin-top: 10px;
        }
        .button {
            padding: 8px 15px;
            text-align: center;
            font-size: 14px;
            font-weight: bold;
            border-radius: 5px;
            cursor: pointer;
            color: white;
            text-decoration: none; /* Remove underline */
        }
        .button.git {
            background-color: #24292e; /* GitHub button color */
        }
        .button.zenodo {
            background-color: #0079d3; /* Zenodo button color */
        }
        .button:hover {
            opacity: 0.9;
        }
    </style>
</head>
<body>
    <h1>Gallery with Links</h1>
    <p>Inspired by the EDS Book website (https://edsbook.org/notebooks/gallery), the following gallery has clickable links with directions to <strong>Talks</strong>, the <strong>Zenodo Website</strong>, and a <strong>Git Repository</strong>.</p>
    <div class="gallery-container">
        <!-- First Item -->
        <div class="gallery-item">
            <a href="http://localhost:4000/talks/">
                <img src="/images/stats_screenshot.png" alt="Stats Screenshot">
            </a>
            <div class="button-container">
                <a href="https://github.com/example-repo" class="button git">Git</a>
                <a href="https://zenodo.org/example-link" class="button zenodo">Zenodo</a>
            </div>
        </div>
        <!-- Second Item -->
        <div class="gallery-item">
            <a href="http://localhost:4000/talks/">
                <img src="/images/mardi_screenshot.png" alt="Mardi Screenshot">
            </a>
            <div class="button-container">
                <a href="https://github.com/example-repo" class="button git">Git</a>
                <a href="https://zenodo.org/example-link" class="button zenodo">Zenodo</a>
            </div>
        </div>
        <!-- Third Item -->
        <div class="gallery-item">
            <a href="https://zenodo.org/">
                <img src="/images/zenodo_screenshot.png" alt="Zenodo Screenshot">
            </a>
            <div class="button-container">
                <a href="https://github.com/example-repo" class="button git">Git</a>
                <a href="https://zenodo.org/example-link" class="button zenodo">Zenodo</a>
            </div>
        </div>
        <!-- Fourth Item -->
        <div class="gallery-item">
            <a href="https://github.com/OleksandrZadorozhnyiML/StMaRDI/blob/main/notebook_01_alarm_publish.Rmd">
                <img src="/images/portal_screenshot.png" alt="Portal Screenshot">
            </a>
            <div class="button-container">
                <a href="https://github.com/example-repo" class="button git">Git</a>
                <a href="https://zenodo.org/example-link" class="button zenodo">Zenodo</a>
            </div>
        </div>
    </div>
</body>
</html>
# Zenodo Seach
Searching in Zenodo is a logical ‘OR’ and not a logical ‘AND'. We have thus implemented a search option generating URL for an appropriate Zenodo search with __javascript__. Select the desired attributes and click the button to open an external Zenodo page.
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>City Selector</title>
    <style>
        body {
            font-family: Arial, sans-serif;
        }
        .filter-container {
            display: flex;
            flex-wrap: wrap; /* Allows items to wrap to the next row */
            gap: 20px; /* Space between items */
       }
        .filter-group {
            flex: 1 1 calc(50% - 20px); /* Two items per row, accounting for gap */
            box-sizing: border-box;
            margin: 0;
            padding: 15px;
            border-radius: 8px;
        }
        .city-list {
            list-style: none;
            padding: 0;
        }
        .city-list li {
            margin: 10px 0;
        }
        .task-group {
            background-color: #CCE8DB;
        }
        .type-group {
            background-color: #C1D4E3;
        }
        .scope-group {
            background-color: #BEB4D6;
        }
        .truth-group {
            background-color: #FADAE2;
        }
        .license-group {
            background-color: #F8B3CA;
        }
        .missing-group {
            background-color: #CC97C1;
        }
        .temporal-group {
            background-color: #CCE8DB;
        }
        .filter-group h3 {
            margin-top: 5px;
            margin-bottom: 5px;
            font-size: 20px;
        }
        .generate-btn {
            margin-top: 20px;
            padding: 10px 15px;
            background-color: #4CAF50;
            color: white;
            border: none;
            border-radius: 5px;
            cursor: pointer;
        }
        .generate-btn:hover {
            background-color: #45a049;
        }
    </style>
</head>
<body>
    <div class="filter-container">
        <div class="filter-group task-group">
            <h3>Task</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="a1" value="Causal Discovery"> Causal Discovery</li>
                <li><input type="checkbox" id="a2" value="Causal Inference"> Causal Inference</li>
            </ul>
        </div>
        <div class="filter-group type-group">
            <h3>Data Type</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="b1" value="Continuous Data"> Continuous Data</li>
                <li><input type="checkbox" id="b2" value="Discrete Data"> Discrete Data</li>
                <li><input type="checkbox" id="b3" value="Mixed Data"> Mixed Data</li>
                <li><input type="checkbox" id="b4" value="Binary Data"> Binary Data</li>
            </ul>
        </div>
        <div class="filter-group scope-group">
            <h3>Dataset Scope</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="c1" value="Collection"> Collection</li>
                <li><input type="checkbox" id="c2" value="Standalone"> Standalone</li>
            </ul>
        </div>
        <div class="filter-group truth-group">
            <h3>Ground Truth</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="d1" value="Available"> Available</li>
                <li><input type="checkbox" id="d2" value="Partial"> Partial</li>
                <li><input type="checkbox" id="d3" value="Unknown"> Unknown</li>
            </ul>
        </div>
        <div class="filter-group license-group">
            <h3>License</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="e1" value="Research Only"> Research Only</li>
                <li><input type="checkbox" id="e2" value="Open"> Open</li>
            </ul>
        </div>
        <div class="filter-group missing-group">
            <h3>Missing Values</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="f1" value="Complete Data"> Complete Data</li>
                <li><input type="checkbox" id="f2" value="Missing Values"> Missing Values</li>
            </ul>
        </div>
	<div class="filter-group temporal-group">
            <h3>Missing Values</h3>
            <ul class="city-list">
                <li><input type="checkbox" id="g1" value="Static"> Static</li>
                <li><input type="checkbox" id="g2" value="Time Series"> Time Series</li>
            </ul>
        </div>
    </div>
    <button class="generate-btn">Generate URL</button>
    <script src="{{ '/assets/js/generateURL.js' | relative_url }}"></script>
</body>
</html>
* Maybe, we should add a 'static' vs. 'time series' selection as well.
* The implemented Zenodo search works by scanning through all descriptions and searching for perfect matches. In particular, it does not relate to the tags. To not have two contradicting search options, I'd suggest to have the Zenodo Tags only for the subject, e.g., pharmaceutical or weather, and write the characterization in plain text in a tabular fashion at the beginning of each Dataset.
