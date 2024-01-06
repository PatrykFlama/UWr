let mysql = require('mysql');

let con = mysql.createConnection({
    host: "localhost",
    user: "root",
    password: "admin",
    database: "weppo",
});


con.connect(function(err) {
    if (err) throw err;

    // Create the "OSOBA" table
    let createOsobaTable = "CREATE TABLE IF NOT EXISTS OSOBA (ID INT AUTO_INCREMENT PRIMARY KEY, NAME VARCHAR(255), ID_MIEJSCE_PRACY INT, FOREIGN KEY (ID_MIEJSCE_PRACY) REFERENCES MIEJSCE_PRACY(ID))";
    con.query(createOsobaTable, function(err, result) {
        if (err) throw err;
        console.log("OSOBA table created!");
    });

    // Create the "MIEJSCE PRACY" table
    let createMiejscePracyTable = "CREATE TABLE IF NOT EXISTS MIEJSCE_PRACY (ID INT AUTO_INCREMENT PRIMARY KEY, NAME VARCHAR(255))";
    con.query(createMiejscePracyTable, function(err, result) {
        if (err) throw err;
        console.log("MIEJSCE PRACY table created!");
    });

    console.log("Tables created!");
});

// --------------------------------------------------

function createBusiness(businessName, employees){
    try {
        let businessID;

        let sql = `INSERT INTO MIEJSCE_PRACY (NAME) VALUES ('${businessName}')`;
        con.query(sql, function(err, result) {
            if (err) throw err;
            businessID = result.insertId; 
        });

        for(let i = 0; i < employees.length; i++){
            let sql = `INSERT INTO OSOBA (NAME, ID_MIEJSCE_PRACY) VALUES ('${employees[i]}', '${businessID}')`;
            con.query(sql, function(err, result) {
                if (err) throw err;
            });
        }

        console.log("Business added");
    } catch (error) {
        console.error(error);
    }
}

createBusiness("Firma1", ["Jan Kowalski", "Adam Nowak", "Anna Nowak"]);

let sql = "SELECT * FROM MIEJSCE_PRACY";
con.query(sql, function(err, result) {
    if (err) throw err;
    console.log(result);
});