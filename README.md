# phutball-fs
Phutball (philosopher's football) game in F#

See here a description on the game https://en.wikipedia.org/wiki/Phutball

There's no AI involved, players take turns and move either a stone representing a player or the ball (if the move is allowed). Jumped players are removed. The turn is indicated with an arrow (up or down). There is no rebounding from the goal.

I chose to test my engine via a basic form, but the code that defines the board and the game dynamics is separated in a module and can be used with other types of visualisation layers.

![alt tag](http://s15.postimg.org/ald718hu3/phutball.png)
