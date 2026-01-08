# Search Zenodo records (unauthenticated)

Search Zenodo records (unauthenticated)

## Usage

``` r
zenodo_search(query, community = NULL, size = 200, page = 1)
```

## Arguments

- query:

  Optional search query string.

- community:

  Optional community id.

- size:

  Page size for the request.

- page:

  Page number for the request.

## Value

A list parsed from the Zenodo JSON response.
