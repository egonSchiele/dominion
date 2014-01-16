# Dominion

Dominion Simulator. Allows you to test competing strategies and see what works best.

Usage:

    ./dominion [# of iterations -- default 500]

# Strategies

Here's a simple strategy, the "big money" strategy:

```haskell
bigMoney playerId = playerId `buysByPreference` [C.province,
                                                 C.gold,
                                                 C.duchy,
                                                 C.silver,
                                                 C.copper
                                                ]
```

The big money strategy is simple: buy the most expensive victory or treasure card you can, and repeat.

You can give `buysByPreference` a list of all the cards you want to buy, in the order you want to buy them in. `buysByPreference` will then buy as many cards as it can.

Here's another example: big money + smithy:

```haskell
bigMoneySmithy playerId = do
    playerId `D.plays` C.smithy
    playerId `D.buysByPreference` [C.province,
                                   C.gold,
                                   C.duchy,
                                   C.smithy,
                                   C.silver,
                                   C.copper
                                  ]
```

It's just like big money, except now the player buys up smithys too. And the player plays a smithy whenever possible (`plays` will play the action card if it can). So now we can compare Big Money vs Big Money + Smithy. And the results are:

Out of 5000 games:

    player adit won 1867 times using "big money"
    player maggie won 3133 times using "big money + smithy"

Just as a control, lets use the same strategy with both players:

    player adit won 2444 times
    player maggie won 2556 times

So clearly, adding the smithy to big money makes a big difference!
