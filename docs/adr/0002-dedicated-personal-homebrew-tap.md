# Dedicated Personal Homebrew Tap

We decided to distribute `hach` via a dedicated personal Homebrew tap (`jonbaldie/homebrew-tap`) rather than publishing it to `quality-gates/homebrew-tap`. The `quality-gates` tap is restricted to organizational mutation and mess detection tools and hardcodes `quality-gates/*` repository URLs in its automation workflows. A personal tap provides clean organizational separation and delivers the canonical installation path `brew install jonbaldie/tap/hach`.
