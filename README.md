# Belka Router

## Overview

This is a simple URL router for the [Belka Gemini Server](https://github.com/gordonguthrie/belka) that takes URLs from a server and maps them to handler functions.

For an example of how to build the simplest Belka Gemini Server WITHOUT a URL router please see [Belka Example](https://github.com/gordonguthrie/belka-example)

For an example of a Belka Server that uses this router please see [Vega and Altair](https://github.com/gordonguthrie/vega_and_altair)

## Architecture Documents

The [architectural documentation](https://gordonguthrie.github.io/belka_router) for this example is built with [Literate Code Reader](https://gordonguthrie.github.io/literatecodereader)


## Other Belka Family Members

Belka has a templating engine [Belka Templates](https://github.com/gordonguthrie/belka-templates) - it is also used in [Vega and Altair](https://github.com/gordonguthrie/vega_and_altair)


## For Belka Developers

There are a couple of helper functions to make it easier for you when you are integrating the Belka Router into your Gemini server:

* `belka_router:recompile_routes/0`
* `belka_router:toggle_debug/0`

Check 'em out