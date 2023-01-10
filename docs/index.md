# Belka Router

## What is a router?

A router is a bit of code that processes paths in URL. It takes the path, applies some rules to it and binds it to a function.

This application takes this data structure defined in the module `example_routes`:

```erlang

{"/home/:user", ?USERLOGIN, {example_handlers, home}},

```

compiles it into an internal data structure and then matches incoming `gemini://` requests and against that data structure.

NOTE: the module `example_routes` is the routing table for the site `localhost`, you might have multiple sites on the same Belka server each with their own routing tables - `belka_router` handles that for you too.

If the routes doesn't find a match it returns the appropriate error message to the client and if it does if processes the request and calls the handler declared in the route. In this case a function called `home/2` in the module `example_handlers`:

```erlang

home(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in example:home Path is ~p Vals is ~p~n", [Path, Vals]),
	["20 text/gemini\r\nhome\r\n"].

```

In this case the valid root is any logged in user with a 2 segment URL where the first segment is `home`.

You can get by without a router quite easily as we can see in the [Belka Example](http://gordonguthrie.github.io/belka_example) but you have to continually reimplement ***page not found*** and ***you must be logged in*** and ***you are not an administrator***. This router takes care of all that for you.



### Important!

if you add the router to your own app - ADD YOUR OWN SALT IN THE CONFIGURATION - otherwise your nonces won't be nonces at all.

## Sequence Diagram

This diagram shows the normal sequence of using the route (the router will be started by the top supervisor in your application). It shows three use cases:

* initiation of the server
* an invalid incoming request
* a valid incoming request
^

```
+-------------+                                                +---------+                                                            +-------------+ +---------+ +-----------+
| BelkaServer |                                                | Router  |                                                            | ConfigFile  | | Routes  | | Handlers  |
+-------------+                                                +---------+                                                            +-------------+ +---------+ +-----------+
       |                                                            |                                                                        |             |            |
       |                                                            | start                                                                  |             |            |
       |                                                            |------                                                                  |             |            |
       |                                                            |     |                                                                  |             |            |
       |                                                            |<-----                                                                  |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | read list of module/function that supplies routes for each site        |             |            |
       |                                                            |----------------------------------------------------------------------->|             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | read cryptographic salts                                               |             |            |
       |                                                            |----------------------------------------------------------------------->|             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | read list of administrators                                            |             |            |
       |                                                            |----------------------------------------------------------------------->|             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | loop over sites calling module/function to get list of routes for each site          |            |
       |                                                            |------------------------------------------------------------------------------------->|            |
       |                                                            |                                                                        |             |            |
       |                                                            | compile the routes                                                     |             |            |
       |                                                            |-------------------                                                     |             |            |
       |                                                            |                  |                                                     |             |            |
       |                                                            |<------------------                                                     |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | open for business                                                      |             |            |
       |                                                            |------------------                                                      |             |            |
       |                                                            |                 |                                                      |             |            |
       |                                                            |<-----------------                                                      |             |            |
       |                                                            |                                                                        |             |            |
       | please handle this INVALID request on this site            |                                                                        |             |            |
       |----------------------------------------------------------->|                                                                        |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | look up the routes for this site                                       |             |            |
       |                                                            |---------------------------------                                       |             |            |
       |                                                            |                                |                                       |             |            |
       |                                                            |<--------------------------------                                       |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | checks if the request is valid                                         |             |            |
       |                                                            |-------------------------------                                         |             |            |
       |                                                            |                              |                                         |             |            |
       |                                                            |<------------------------------                                         |             |            |
       |                                                            |                                                                        |             |            |
       |              request invalid please use this error handler |                                                                        |             |            |
       |<-----------------------------------------------------------|                                                                        |             |            |
       |                                                            |                                                                        |             |            |
       | please do the error handling on my request                 |                                                                        |             |            |
       |----------------------------------------------------------->|                                                                        |             |            |
       |                                                            |                                                                        |             |            |
       |                          please return this error response |                                                                        |             |            |
       |<-----------------------------------------------------------|                                                                        |             |            |
       |                                                            |                                                                        |             |            |
       | please handle this VALID request request on this site      |                                                                        |             |            |
       |----------------------------------------------------------->|                                                                        |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | look up the routes for this site                                       |             |            |
       |                                                            |---------------------------------                                       |             |            |
       |                                                            |                                |                                       |             |            |
       |                                                            |<--------------------------------                                       |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | checks if the request is valid                                         |             |            |
       |                                                            |-------------------------------                                         |             |            |
       |                                                            |                              |                                         |             |            |
       |                                                            |<------------------------------                                         |             |            |
       |                                                            |                                                                        |             |            |
       |                                                            | please handle this request                                             |             |            |
       |                                                            |-------------------------------------------------------------------------------------------------->|
       |                                                            |                                                                        |             |            |
       |                                                            |                                                                        |         here is my reply |
       |                                                            |<--------------------------------------------------------------------------------------------------|
       |                                                            |                                                                        |             |            |
       |                                 please serve this response |                                                                        |             |            |
       |<-----------------------------------------------------------|                                                                        |             |            |
       |                                                            |                                                                        |             |            |

%```

## Understanding what the router does

The easiest way to do this is to read the `example_routes` and `example_hander` modules side by side.

* [Example Routes](./example_routes.html)
* [Example Handlers](./example_handlers.html)

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
