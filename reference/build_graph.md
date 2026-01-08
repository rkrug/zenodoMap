# Build nodes and edges for the network graph

Build nodes and edges for the network graph

## Usage

``` r
build_graph(
  records,
  depth = 0,
  max_expand = 300,
  allowed_relations = NULL,
  community_ids = NULL,
  community_only = FALSE,
  title_map = NULL,
  concept_map = NULL
)
```

## Arguments

- records:

  List of Zenodo records.

- depth:

  Expansion depth for related Zenodo records.

- max_expand:

  Maximum records to expand into.

- allowed_relations:

  Relation filter vector.

- community_ids:

  Community record ids.

- community_only:

  Logical; keep only community-to-community links.

- title_map:

  Optional id to title map.

- concept_map:

  Optional concept id to record id map.

## Value

List with \`nodes\` and \`edges\` data frames.
