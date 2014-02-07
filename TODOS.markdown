- add some kind of way to compete all strategies against each other at once we I can figure out which is the best one
- avg # of VPs per strategy
- graphs (one dot per iteration)
- add intrigue cards

- someone suggested that `CardEffect` is going to get cluttered with all these different types of effects, so I should have one way to model all of them:

    SpecialEffect (PlayerId -> Dominion (Maybe Followup))

The problem is, all those special effects have to get coded in somewhere. SO either they are in the CardEffect type, or I have to make a new type for Followup that returns `AdventurerFollowup` or `LibraryFollowup` depending on the card. So something is going to get cluttered.
