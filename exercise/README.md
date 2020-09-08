# Tic-Tac-Toe in Miso

Inspired by [this course in React](https://egghead.io/lessons/javascript-create-a-2-dimensional-grid-for-our-tic-tac-toe-game).

The `exercise` folder contains the initial implementation of Tic-Tac-Toe, the game in which users take turns writing `X` or `O` on a square grid, and the first one to get a full row, column, or diagonal, wins.

The initial implementation has a simple data model with a grid, each of them being a (possibly empty) square. We also provide the `hasWinner` function to check whether one of the players satisfies the winning conditions.

## 1. Show the board

The view has been divided in different sections: header, grid, and alert; each with a corresponding view function. This first exercise focus on showing the content of the grid, since right now each square simply shows a dot. Rework the `cell` function to:

- Show `X` or `O` depending on the content of the square,
- Disable the button if there's already some content there.

You can test that your function works by setting the initial state to `aGrid` instead of `emptyGrid`.

## 2. Click on the tiles

The `onClick` event in each button of the grid sends a `ClickSquare` action with the row and column index of the square the user has clicked. Complete the logic to update the model with the corresponding information.

This task involves more than the view and action handler, you need to extend the model to keep track of which is the _current_ player. Update the `gridView` to somehow indicate who the current player is. You can use some of the [colors provided by Bootstrap](https://getbootstrap.com/docs/4.5/utilities/colors/#color) by changing the `class` attribute of the corresponding node.

## 3. Finish the game

What was the `alertView` for? To show the winner, of course! Extend the application to detect when you have a winner (remember, we provide `hasWinner` for that purpose), and in affirmative case:

- Show the appropiate message in the alert,
- Disable any further clicks.

Furthermore, rework the code to _not_ show the alert if there is not yet a winner.

## 4. New game

Uncomment the line `, newGameView m` in the `viewModel` function. After rebuilding, a new bar with two text boxes and one big _New game_ button should appear. Let's forget about the former for now; your task is to start a new game when the button is clicked.

For an extra challenge, try enabling the button _only_ when the previous game has finished. Be careful, since not every game has a winner, so you need to add additional code to detect that case.

## 5. Player names

The next step is putting the text boxes in use. The idea is that instead of generic _Player 1_ and _Player 2_, the UI should show those names everywhere they are needed. This includes the big elements located next at both sides of the grid which indicate whose turn it is, and also the alert for winners.

## 6. Storing stats locally

Uncomment the line `, statsView m` in the `viewModel` function. Right now it shows some fake stats of previews games. Your task is to make those statistics real, saving information about the games played in your application. Once you get it working on a single reload, make those stats permanent by keeping them in local storage.

Remember the following rules related to local storage:

- If you recompile your application, it gets a new route, so browsers do not associate the old stored data with the new page.
- If you need to clear your local storage, the _Developer Tools_ in most browsers (definitely in Firefox and Chrome) give you a way to do so throught their _Storage_ tab.

## 7. Add some flags

The `newGameView` function has some form components commented out; it's time to play with them! In particular, we would like those dropdowns to include a list of every country. You should of course _not_ hardcode them, you should use the [REST Countries](https://restcountries.eu/) service instead.

The simplest endpoint to use is [`/all`](https://restcountries.eu/#api-endpoints-all). Our suggestion is for you to surf directly to [`https://restcountries.eu/rest/v2/all`](https://restcountries.eu/rest/v2/all) and explore the data yourself. Then use Servant to bring that information into your application.

The data of each country includes a URL to their flag. Wouldn't it be nice to show it next to the name of each player? The World Championship of Tic-Tac-Toe!

*Extra help*: if case you get stuck translating the REST calls and JSON documents into Servant routes and Haskell types, we have prepared a `flags` example which simply loads the list, fills a dropdown with the data, and shows the flag for the selected country.

## 8. Different routes

Instead of showing the grid and the stats in one single page, introduce a separate route for the latter, available at `#stats`. Provide a link to it in the navigation bar.

A more challenge variant involves keeping the _New game_ bar, and going back to the grid view when the corresponding button is pressed. You can use the [History API](https://developer.mozilla.org/en-US/docs/Web/API/History) available from [Miso](https://haddocks.haskell-miso.org/Miso-Subscription-History.html).