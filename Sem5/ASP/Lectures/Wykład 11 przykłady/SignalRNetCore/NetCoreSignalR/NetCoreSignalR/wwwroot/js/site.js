var connection = new signalR.HubConnectionBuilder().withUrl("/chatHub").build();

connection.on("DisplayMessage", function (name, message) {
    // Html encode display name and message. 
    var encodedName = $('<div />').text(name).html();
    var encodedMsg = $('<div />').text(message).html();
    // Add the message to the page. 
    $('#discussion').append('<li><strong>' + encodedName
        + '</strong>:&nbsp;&nbsp;' + encodedMsg + '</li>');
});

connection.start().then(function () {

    $('#sendmessage').click(function () {

        var message = $('#message').val();

        // Call the Send method on the hub. 
        connection.invoke("SendMessage", message);

        // Clear text box and reset focus for next comment. 
        $('#message').val('').focus();
    });

}).catch(function (err) {
    return console.error(err.toString());
});

document.getElementById("sendButton").addEventListener("click", function (event) {
    var user = document.getElementById("userInput").value;
    var message = document.getElementById("messageInput").value;
    connection.invoke("SendMessage", user, message).catch(function (err) {
        return console.error(err.toString());
    });
    event.preventDefault();
});

