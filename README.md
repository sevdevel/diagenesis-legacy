# diagenesis-legacy

The original `diagenesis` 1.0.0 (Filip Meysman, NIOZ; GPL) as used with R 3.x, kept unchanged in behaviour.

- `releases/diagenesis_1.0.0.zip`: the original Windows binary (built under R 3.1.1).
- `R/`, `man/`, `DESCRIPTION`, `NAMESPACE`: source recovered from that binary (comments and original formatting are lost).

For current R versions use the updated package: [diagenesis](https://github.com/sevdevel/diagenesis).

## Install (R 3.x)

```r
# binary (Windows): download releases/diagenesis_1.0.0.zip, then
install.packages("diagenesis_1.0.0.zip", repos = NULL, type = "win.binary")
# or from source
remotes::install_github("sevdevel/diagenesis-legacy")
```
