# Belka Router

## What Is A Router?

A router is a bit of code that processes paths in URL. It takes the path, applies some rules to it and binds it to a function.

```
+-------------+                                    +---------+                                      +-------------+ +-------------+ +---------+
| BelkaServer |                                    | Router  |                                      | ConfigFile  | | RoutesFile  | | Handler |
+-------------+                                    +---------+                                      +-------------+ +-------------+ +---------+
       |                                                |                                                  |               |             |
       |                                                | start                                            |               |             |
       |                                                |------                                            |               |             |
       |                                                |     |                                            |               |             |
       |                                                |<-----                                            |               |             |
       |                                                |                                                  |               |             |
       |                                                | read module/function that supplies routes        |               |             |
       |                                                |------------------------------------------------->|               |             |
       |                                                |                                                  |               |             |
       |                                                | read cryptographic salts                         |               |             |
       |                                                |------------------------------------------------->|               |             |
       |                                                |                                                  |               |             |
       |                                                | read list of administrators                      |               |             |
       |                                                |------------------------------------------------->|               |             |
       |                                                |                                                  |               |             |
       |                                                | call module/function to get list of routes       |               |             |
       |                                                |----------------------------------------------------------------->|             |
       |                                                |                                                  |               |             |
       |                                                | compile the routes                               |               |             |
       |                                                |-------------------                               |               |             |
       |                                                |                  |                               |               |             |
       |                                                |<------------------                               |               |             |
       |                                                |                                                  |               |             |
       |                                                | open for business                                |               |             |
       |                                                |------------------                                |               |             |
       |                                                |                 |                                |               |             |
       |                                                |<-----------------                                |               |             |
       |                                                |                                                  |               |             |
       | please handle this invalid request             |                                                  |               |             |
       |----------------------------------------------->|                                                  |               |             |
       |                                                |                                                  |               |             |
       |                                                | checks if the request is valid                   |               |             |
       |                                                |-------------------------------                   |               |             |
       |                                                |                              |                   |               |             |
       |                                                |<------------------------------                   |               |             |
       |                                                |                                                  |               |             |
       |      request invalid please send this response |                                                  |               |             |
       |<-----------------------------------------------|                                                  |               |             |
       |                                                |                                                  |               |             |
       | please handle this valid request               |                                                  |               |             |
       |----------------------------------------------->|                                                  |               |             |
       |                                                |                                                  |               |             |
       |                                                | checks if the request is valid                   |               |             |
       |                                                |-------------------------------                   |               |             |
       |                                                |                              |                   |               |             |
       |                                                |<------------------------------                   |               |             |
       |                                                |                                                  |               |             |
       |                                                | please handle this request                       |               |             |
       |                                                |------------------------------------------------------------------------------->|
       |                                                |                                                  |               |             |
       |                                                |                                                  |            here is my reply |
       |                                                |<-------------------------------------------------------------------------------|
       |                                                |                                                  |               |             |
       |                     please serve this response |                                                  |               |             |
       |<-----------------------------------------------|                                                  |               |             |
       |                                                |                                                  |               |             |

When the belka server receives a `gemini://` request it



 <div>
 {% for item in site.data.contents.toc %}
     <h3>{{ item.title }}</h3>
       <ul>
         {% for entry in item.subfolderitems %}
           <li><a href="{{ entry.url }}">{{ entry.page }}</a></li>
         {% endfor %}
       </ul>
   {% endfor %}
 </div>
