# Search Zenodo records (authenticated)

Search Zenodo records (authenticated)

## Usage

``` r
zenodo_search_auth(query, community = NULL, size = 200, page = 1, token = NULL)
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

- token:

  Zenodo API token.

## Value

A list parsed from the Zenodo JSON response.
