#!/bin/sh

# This will get the latest version of the uv installer from astral

# uv
curl -LsSf https://astral.sh/uv/install.sh > uv.sh && chmod +x uv.sh

# ruff
curl -LsSf https://astral.sh/ruff/install.sh > ruff.sh && chmod +x ruff.sh


