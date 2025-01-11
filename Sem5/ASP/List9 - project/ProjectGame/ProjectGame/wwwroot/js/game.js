// Please see documentation at https://learn.microsoft.com/aspnet/core/client-side/bundling-and-minification
// for details on configuring this project to bundle and minify static web assets.

// Write your JavaScript code.
const connection = new signalR.HubConnectionBuilder()
    .withUrl("/gamehub")
    .build();

connection.start().then(() => {
    connection.invoke("JoinGame", gameId);

    connection.on("PlayerJoined", (players) => {
        console.log("Players:", players);
    });

    connection.on("GameState", (board, currentPlayer) => {
        console.log("GameState", board);
        updateBoard(board);
        document.getElementById("current-player").innerText = currentPlayer;
    });

    connection.on("MoveMade", (row, col, symbol, nextPlayer) => {
        console.log("Movemade", row, col, symbol);
        updateCell(row, col, symbol);
        document.getElementById("current-player").innerText = nextPlayer;
    });

    connection.on("PlayerRole", (role) => {
        console.log("Role assigned", role);
        document.getElementById("player-role").innerText = role;
    })

    connection.on("GameOver", (winner) => {
        console.log("gameover", winner);
        document.getElementById("game-result").innerText = `Game Over! Winner: ${winner}`;
    });

    document.querySelectorAll(".cell").forEach(cell => {
        cell.addEventListener("click", () => {
            const row = cell.dataset.row;
            const col = cell.dataset.col;
            console.log("makemove", gameId, row, col);
            connection.invoke("MakeMove", gameId, row, col);
        });
    });
});

function updateBoard(board) {
    console.log("updateBoard", board);

    document.querySelectorAll(".cell").forEach(cell => {
        const row = cell.dataset.row;
        const col = cell.dataset.col;
        cell.innerText = board[row][col] || '';
    });
}

function updateCell(row, col, symbol) {
    console.log("updateCell", row, col, symbol);

    const cell = document.querySelector(`.cell[data-row="${row}"][data-col="${col}"]`);
    cell.innerText = symbol;
}