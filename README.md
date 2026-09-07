# FUTURE Business Cockpit — R Backend

This is the separate R service for the already deployed `FBC_R_INTEGRATION_STAGE1` frontend.

## What is live in this package

- `GET /health`
- `POST /analyze`
- request schema: `FBC_P1_COCKPIT_REQUEST_1.0`
- response schema: `fbc_decision_payload_v1`
- P0 factual/current-state path
- 2026 tax orientation
- factual billable-hours separation
- Business Break-even
- Employee Break-even
- current financing / capital-service orientation
- Evidence Gate: no confirmed bounds -> no optimizer recommendation

The R production modules 09/10/11/18/19/21/22/26/27C/29/34/35/36/37/38/39 and the fast engine are bundled.

## Deliberate production gate

`FBC_ENABLE_P1_RUNNER` defaults to `false`.

The validated production MC CSV is not present in the user's Library under the expected filename, so this package does **not** fabricate it.

Also, the bundled runner 39 still accepts fixed monthly tax/pension context inside its candidate evaluator. P0 uses the dynamic 2026 orientation adapter; P1/P2 remains blocked until that adapter is wired through the optimizer and re-run in a real R runtime.

This is intentional: no fake recommendation fallback.

## Run locally

Requires R 4.6.x.

```r
install.packages(c("plumber","jsonlite"))
pr <- plumber::plumb("api.R")
pr$run(host="0.0.0.0", port=8000)
```

Health:

`GET http://localhost:8000/health`

## Docker

```bash
docker build -t fbc-r-backend .
docker run --rm -p 8000:8000 fbc-r-backend
```

## Connect to Vercel

After deploying this R service, set in the existing Vercel project:

`R_BACKEND_ANALYZE_URL=https://YOUR-R-HOST/analyze`

Optional shared secret:

R host:
`FBC_R_API_KEY=...`

Vercel:
`R_BACKEND_API_KEY=...`

No frontend rebuild is required just to change the R URL.

## MC data

For P1/P2 later, put the validated file at:

`data/processed/fbc_monte_carlo_draws.csv`

or set:

`FBC_MC_FILE=/absolute/path/to/fbc_monte_carlo_draws.csv`

Do not use test/synthetic MC draws in production.
