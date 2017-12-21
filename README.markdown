# Set-Game

This is a simple implementation of the game SET.

## Dependencies

To install dependencies with quicklisp:

    CL-USER> (ql:quickload '("hunchentoot" "hunchensocket"))

## Usage

To load:

    CL-USER> (asdf:load-system '#:set-game)

To start the server:

    CL-USER> (set-game:start-game-server :port 8080)

play a game connect to `http://localhost:8080`. Click “New Game” to
start a new game. This gives a game code which other players may use
to connect to the game. The game begins when every player enters their
name and clicks 'Go'. Click on cards to identify a set. A set is three
cards such that each attribute (colour, shape, shading, number) is
either unique or shared, that is, you cannot have exactly two cards
the same for any attribute. Alternatively assign some number 0, 1, or
2 to each value for each attribute and then each card may be assigned
to a member of `Z_3^4` (vectors of four elements of the field of
integers mod 3) and three cards form a set if they are in arithmetic
progression in that vector space. There is no feature to add cards if
there are no sets and end-of-game is not detected.

## License

Licensed under the GPL3 License.
