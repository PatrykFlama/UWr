const connection = new signalR.HubConnectionBuilder()
    .withUrl("/gameHub")
    .build();

async function fetchRooms(playerName = "") {
    try {
        await connection.start();

        // Call the SignalR method with the playerName filter
        const rooms = await connection.invoke("GetRooms", playerName);

        const roomsBody = document.getElementById("rooms-body");
        roomsBody.innerHTML = "";

        if (rooms.length > 0) {
            rooms.forEach(room => {
                const row = document.createElement("tr");
                row.innerHTML = `
                        <td>${room}</td>
                        <td>
                            <a href="/Game/Play?gameId=${room}" class="btn btn-primary">Join</a>
                        </td>
                    `;
                roomsBody.appendChild(row);
            });
        } else {
            roomsBody.innerHTML = `
                    <tr>
                        <td colspan="2">No rooms available.</td>
                    </tr>
                `;
        }
    } catch (err) {
        console.error("Error fetching rooms:", err);
        document.getElementById("rooms-body").innerHTML = `
                <tr>
                    <td colspan="2">Error loading rooms.</td>
                </tr>
            `;
    }
}

// Search functionality
document.getElementById("searchButton").addEventListener("click", () => {
    const playerName = document.getElementById("playerName").value;

    // Update the query string
    const url = new URL(window.location.href);
    if (playerName) {
        url.searchParams.set("playerName", playerName);
    } else {
        url.searchParams.delete("playerName");
    }
    window.history.pushState({}, "", url);

    // Fetch rooms with the playerName filter
    fetchRooms(playerName);
});

// Initial fetch on page load
const params = new URLSearchParams(window.location.search);
const initialPlayerName = params.get("playerName") || "";
document.getElementById("playerName").value = initialPlayerName;
fetchRooms(initialPlayerName);
