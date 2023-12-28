let mysql = require('mysql');

let con = mysql.createConnection({
    host: "localhost",
    user: "root",
    password: "admin",
    database: "weppo",
});

function drop_table() {
    let sql = "DROP TABLE IF EXISTS PEARSON;";
    con.query(sql, function(err, result) {
        if (err) throw err;
        console.log("Table dropped");
    });
    let sql2 = "DROP TABLE IF EXISTS PEARSON2;";
    con.query(sql2, function(err, result) {
        if (err) throw err;
        console.log("Table dropped");
    });
}

function create_table() {
    let sql = "CREATE TABLE PEARSON (id INT AUTO_INCREMENT PRIMARY KEY, name VARCHAR(255), surname VARCHAR(255), age INT, sex VARCHAR(1));";
    con.query(sql, function(err, result) {
        if (err) throw err;
        console.log("Table created");
    });

    /* //! To create sequence in PostgreSQL, which does not exist in MySQL since it is using AUTO_INCREMENT:
        CREATE SEQUENCE seq_person_id
        START WITH 1
        INCREMENT BY 1;

        INSERT INTO PEARSON(id, name, surname, age, sex)
        VALUES (NEXTVAL('seq_person_id'), 'John', 'Doe', 30, 'M');
    */

    // simulation of sequence in MySQL:
    let sql2 = "CREATE TABLE PEARSON2 (id INT PRIMARY KEY, name VARCHAR(255), surname VARCHAR(255), age INT, sex VARCHAR(1));";
    con.query(sql2, function(err, result) {
        if (err) throw err;
        console.log("Table2 created");
    });
}

function add_person(name, surname, age, sex) {
    let sql = `INSERT INTO PEARSON (name, surname, age, sex) VALUES ('${name}', '${surname}', '${age}', '${sex}')`;
    con.query(sql, function(err, result) {
        if (err) throw err;
        console.log("Person added");
    });

    // simulation of sequence in MySQL:
    let sql2 = `INSERT INTO PEARSON2(id, name, surname, age, sex) SELECT IFNULL((SELECT MAX(id) FROM PEARSON), 0) + 1, '${name}', '${surname}', '${age}', '${sex}';`;
    con.query(sql2, function(err, result) {
        if (err) throw err;
        console.log("Person added");
    });
}

con.connect(function(err) {
    if (err) throw err;
    console.log("Connected!");

    drop_table();
    create_table();

    const pearsons = [
        ["John", "Doe", 30, "M"],
        ["Jane", "Doe", 25, "F"],
        ["John", "Smith", 40, "M"],
        ["Jane", "Smith", 35, "F"],
    ];

    pearsons.forEach(pearson => {
        add_person(pearson[0], pearson[1], pearson[2], pearson[3]);
    });

    con.query("SELECT * FROM PEARSON", function(err, result, fields) {
        if (err) throw err;
        console.log("------------ SELECT * FROM PEARSON ------------");
        console.log(result);
    });

    con.query("SELECT * FROM PEARSON WHERE surname = 'Doe'", function(err, result, fields) {
        if (err) throw err;
        console.log("------------ SELECT * FROM PEARSON WHERE surname = 'Doe' ------------");
        console.log(result);
    });
});
