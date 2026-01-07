# Shiny_ZenodMap

Minimal Shiny app to explore Zenodo records as a network graph.

## Run

```r
shiny::runApp()
```

## Notes

## Usage

- Click "Fetch metadata" to load records and populate the keyword list.
- Adjust graph options (depth, relations, keywords), then click "Refresh graph".
- Fetching metadata also triggers a graph refresh.

## Notes

- Default community id is `ipbes` (IPBES community). You can add extra query terms.
- Expansion depth is selectable (0-2). Expansion follows Zenodo DOIs only and is capped for performance.
- Zenodo limits unauthenticated page size to 25; add a token to raise it to 100 per page.
- Keyword filter is populated from record keywords and supports multi-select.
- Relation type filter is optional; select "All" to include every relation.
- Nodes are colored by community vs external deposits; enable "Only community-to-community links" to hide external nodes/edges.
