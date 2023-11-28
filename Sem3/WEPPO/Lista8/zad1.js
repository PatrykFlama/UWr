var mysql = require('mysql');

var config = mysql.createConnection({
  host: "localhost",
  user: "yourusername",
  password: "yourpassword"
});

config.connect(function(err) {
  if (err) throw err;
  console.log("Connected!");
  config.query("CREATE DATABASE mydb", function (err, result) {
    if (err) throw err;
    console.log("Database created");
  });
});
