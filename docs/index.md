# Belka Router

## What Is A Router?

A router is a bit of code that processes paths in URL. It takes the path, applies some rules to it and binds it to a function.

This application takes this data struture defined in the module `example_routes`:

```erlang

{"/home/:user",  ?USERLOGIN,   {example_handlers, home}},

```

compiles it into an internal data structure and then matches incoming `gemini://` requests and against that data structure.

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
```

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
