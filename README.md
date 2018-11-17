# hill-climbing

[![Hackage](https://img.shields.io/hackage/v/hill-climbing.svg)](https://hackage.haskell.org/package/hill-climbing)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/hill-climbing/badge/lts)](http://stackage.org/lts/package/hill-climbing)
[![Stackage Nightly](http://stackage.org/package/hill-climbing/badge/nightly)](http://stackage.org/nightly/package/hill-climbing)
[![Build status](https://secure.travis-ci.org/ChrisPenner/hill-climbing.svg)](https://travis-ci.org/ChrisPenner/hill-climbing)

Hill climbing algorithm using Store Comonads

This provides some fun functions for finding local maxima in simple topologies

`showSteps delta n t` will show `n` steps it takes towards the local maxima
checking the surrounding values displaced by `delta` orthogonally from the
current position as a nested tuple `((x, y), value)`

```haskell
λ> showSteps 0.1 10 (buildTopology (0.3, 0.2))
[((0.3,0.2),0.2896295),((0.4,0.2),0.3816559),((0.5,0.2),0.46986896),((0.6,0.2),0.5533
872),((0.70000005,0.2),0.63137627),((0.8000001,0.2),0.7030568),((0.9000001,0.2),0.767
7126),((1.0000001,0.2),0.8246977),((1.1000001,0.2),0.8734426),((1.2000002,0.2),0.9134
6043)]
```
