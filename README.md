# Early-Life Heat Exposure and Adult Outcomes

This repository reproduces five tables and five figures for the analysis of
early-life temperature exposure and adult health outcomes in Italy and Croatia.
The package follows a strict separation between immutable inputs, generated
intermediate data, analysis-ready data, code, and final outputs.

## Reproduced outputs

Tables:

1. Croatian health regressions, Tmin >= 20 C.
2. Italian health regressions, Tmin >= 20 C.
3. Italian health regressions, Tmax >= 30 C.
4. Italian summary statistics.
5. Italian Tmin >= 20 C regressions with CR1 inference and wild-cluster
   bootstrap p-values.

Figures:

1. Italian SHARE early-life regions.
2. Italian SHARE-region/E-OBS intersections.
3. Croatian SHARE early-life regions.
4. Croatian SHARE-region/E-OBS intersections.
5. Italian Tmin-threshold sensitivity analysis for 18--22 C.

## Directory structure

```text
01_data/01_raw/          Immutable provider files; restricted/large files ignored by Git
01_data/02_processed/    Deterministically generated SHARE and temperature files
01_data/03_analysis/     Person-level analysis files; ignored by Git
02_code/                 Numbered analysis, table, figure, and validation scripts
03_outputs/              Final tables, figures, LaTeX fragments, and estimates
```

## Data access and confidentiality

SHARE microdata cannot be redistributed publicly. Researchers must register
with SHARE, accept its conditions of use, and download Release 9.0.0. The exact
raw filenames and locations are checked by
`02_code/09_validation/00_validate_raw_inputs.R`.
The `.gitignore` file prevents restricted person-level files from entering a
public Git repository. E-OBS NetCDF files are also excluded because of size;
they must be obtained from the data provider.

The locally delivered project contains the authorized files needed for this
replication. Before sharing the repository, run `git status` and confirm that
no restricted or large provider files are staged.

## Software

- R 4.x
- Python 3 with Pillow, used only for deterministic whitespace cropping
- A LaTeX distribution with `booktabs` and `graphicx`
- R packages: `data.table`, `haven`, `ggplot2`, `sf`, `scales`, `dplyr`,
  `ncdf4`, and `terra`

Package dependencies can be checked with:

```bash
Rscript 02_code/00_config/check_dependencies.R
```

## Reproduction

Run all commands from the repository root:

```bash
Rscript run_all.R
```

The master script starts only from provider files in `01_data/01_raw`. It
extracts and combines the E-OBS NetCDF files, constructs exact area-weighted
regional exposure, prepares both SHARE country samples, creates the
analysis-ready files, estimates all models, formats the tables, regenerates the
five figures, and executes validation checks. Files already present in
`02_processed` or `03_analysis` are outputs of these scripts, not required
starting inputs. The wild-cluster bootstrap uses 9,999 replications and the
fixed seed `20260926`.

The analytical geography consists of 20 SHARE early-life regions in each
country. Official NUTS 2024 geometries are used only as boundary components:
the relevant NUTS polygons are first dissolved into the SHARE regions, and the
E-OBS grid is then intersected directly with those SHARE-region polygons.
Daily temperatures are therefore calculated from exact grid-overlap areas at
the same regional level used for exposure assignment and region fixed effects.

## Reproducibility safeguards

- No script writes into `01_data/01_raw`.
- All paths are relative to the project root.
- Merges test uniqueness of their keys.
- Temperature windows are checked for complete 12-month coverage.
- Clustered inference uses 20 birth-region clusters.
- Outcome-specific complete-case sample sizes are validated.

The PNG is the canonical sensitivity-figure artifact. Its PDF rendering is
visually equivalent but is omitted from byte-level checksum validation because
the PDF device embeds a creation timestamp.

No separate log directory is created. Validation results are printed directly
to the terminal at the end of `run_all.R`.

## Sharing on GitHub

The repository needs only this README, the code, and the final outputs. Before
sharing it with a supervisor, run:

```bash
git status
git add --dry-run .
Rscript 02_code/09_validation/01_validate_outputs.R
```

Do not force-add ignored SHARE microdata, person-level generated data, E-OBS
NetCDF files, or large generated weather files. A recipient must place legally
obtained provider files in `01_data/01_raw` before running `run_all.R`.
