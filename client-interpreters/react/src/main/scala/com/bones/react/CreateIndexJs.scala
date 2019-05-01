package com.bones.react

import com.bones.data.Value.BonesSchema
import com.bones.react.CreateReactFile.ReactFile

object CreateReactFiles {

  type IndexJs = String
  type IndexHtml = String
  type ReactComponent = String

  def fromSchemas(bonesSchemas: List[BonesSchema[_]]): (IndexJs, IndexHtml, ReactComponent) = {
    val components = bonesSchemas.map(CreateReactFile.convert)

    val componentFile = components.map(_.contents).mkString("\n")

    val indexJs = toIndexJs(components)
    val indexHtml = toIndexHtml(components)
    (indexJs, indexHtml, componentFile)

  }

  private def toIndexJs(createReactFile: List[ReactFile]): IndexJs = {

    val render = createReactFile.map(f => s"ReactDOM.render(<${f.crudClassName} />, document.getElementById('${f.labelId}'));").mkString("\n")

    s"""
       |${render}
       |
     """.stripMargin

  }

  private def toIndexHtml(reactFile: List[ReactFile]): IndexHtml = {

    val divs = reactFile.map(f => s"""<div id="${f.labelId}"></div>""").mkString

    val menu = s"""<div class="ui top attached tabular menu">""" +
      reactFile.headOption.map(f => s"""<a class="active item" data-tab="${f.labelId}">${f.crudClassName}</a>""").getOrElse("") +
      reactFile.tail.map(f => s"""<div class="item" data-tab="${f.labelId}">${f.crudClassName}</div>""").mkString +
      s"""</div>"""

    val contents =
      reactFile.headOption.map(f => s"""<div class="ui bottom attached active tab segment" data-tab="${f.labelId}"><div id="${f.labelId}"></div></div>""").getOrElse("") +
      reactFile.tail.map(f => s"""<div class="ui bottom attached tab segment" data-tab="${f.labelId}"><div id="${f.labelId}"></div></div>""").mkString("\n")

    val readyFunction = "$(document).ready(function() { $('.menu .item').tab(); })"

    s"""
       |<!DOCTYPE html>
       |<html lang="en">
       |  <head>
       |    <meta charset="utf-8" />
       |    <link rel="shortcut icon" href="%PUBLIC_URL%/favicon.ico" />
       |    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/semantic-ui/dist/semantic.min.css" crossorigin="anonymous">
       |    <script crossorigin src="https://unpkg.com/react@16/umd/react.production.min.js"></script>
       |    <script crossorigin src="https://unpkg.com/react-dom@16/umd/react-dom.production.min.js"></script>
       |    <script src="https://unpkg.com/@babel/standalone/babel.min.js"></script>
       |    <script
       |            src="https://code.jquery.com/jquery-3.1.1.min.js"
       |            integrity="sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8="
       |            crossorigin="anonymous"></script>
       |    <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js"></script>
       |    <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/react-datepicker/2.5.0/react-datepicker.min.js"></script>
       |    <script type="text/babel" src="index.js"></script>
       |    <meta
       |      name="viewport"
       |      content="width=device-width, initial-scale=1, shrink-to-fit=no"
       |    />
       |    <meta name="theme-color" content="#000000" />
       |    <!--
       |      manifest.json provides metadata used when your web app is added to the
       |      homescreen on Android. See https://developers.google.com/web/fundamentals/web-app-manifest/
       |    -->
       |    <!--
       |      Notice the use of %PUBLIC_URL% in the tags above.
       |      It will be replaced with the URL of the `public` folder during the build.
       |      Only files inside the `public` folder can be referenced from the HTML.
       |
       |      Unlike "/favicon.ico" or "favicon.ico", "%PUBLIC_URL%/favicon.ico" will
       |      work correctly both with client-side routing and a non-root public URL.
       |      Learn how to configure a non-root public URL by running `npm run build`.
       |    -->
       |    <title>Bones App</title>
       |    <script type="text/javascript">
       |      ${readyFunction}
       |    </script>
       |  </head>
       |  <body>
       |    <noscript>You need to enable JavaScript to run this app.</noscript>
       |    ${menu}
       |    ${contents}
       |    <!--
       |      This HTML file is a template.
       |      If you open it directly in the browser, you will see an empty page.
       |
       |      You can add webfonts, meta tags, or analytics to this file.
       |      The build step will place the bundled scripts into the <body> tag.
       |
       |      To begin the development, run `npm start` or `yarn start`.
       |      To create a production bundle, use `npm run build` or `yarn build`.
       |    -->
       |  </body>
       |</html>
       |     """.stripMargin
  }

}
