const mysql = require('mysql');

const conData = {
    host: "localhost",
    user: "root",
    password: "admin",
    database: "weppo",
};

// ---------------------------------------------------------------------------------------------

async function createTable() {
    try {
        const con = mysql.createConnection(conData);

        con.connect(function(err) {
            if (err) throw err;

            const sql = `
                CREATE TABLE IF NOT EXISTS testTable (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    col1 VARCHAR(255),
                    col2 VARCHAR(255)
                )
            `;

            con.query(sql, function (err, result) {
                if (err) throw err;
                console.log("Table created successfully");
                con.end();
            });
        });
    } catch (error) {
        console.error(error);
    }
}

createTable();
    
// ---------------------------------------------------------------------------------------------

async function fetchData() {
    try {
        const con = mysql.createConnection(conData);

        con.connect(function(err) {
            if (err) throw err;

            const sql = 'SELECT * FROM testTable';

            con.query(sql, function (err, result) {
                if (err) throw err;
                console.log(result);
                con.end();
            });
        });
    } catch (error) {
        console.error(error);
    }
}

fetchData();

// ---------------------------------------------------------------------------------------------

async function insertData() {
    try {
        const con = mysql.createConnection(conData);

        con.connect(function(err) {
            if (err) throw err;

            const sql = 'INSERT INTO testTable (col1, col2) VALUES (?, ?)';
            const values = ['val1', 'val2'];

            con.query(sql, values, function (err, result) {
                if (err) throw err;
                const insertedId = result.insertId;
                console.log(insertedId);
                con.end();
            });
        });
    } catch (error) {
        console.error(error);
    }
}

const resID = insertData();
fetchData();

// ---------------------------------------------------------------------------------------------

async function updateData(id, col1, col2) {
    try {
        const con = mysql.createConnection(conData);

        con.connect(function(err) {
            if (err) throw err;

            const sql = 'UPDATE testTable SET col1 = ?, col2 = ? WHERE id = ?';
            const values = [col1, col2, id];

            con.query(sql, values, function (err, result) {
                if (err) throw err;
                console.log(`Updated record with ID: ${id}`);
                con.end();
            });
        });
    } catch (error) {
        console.error(error);
    }
}

updateData(1, 'newVal1', 'newVal2');
fetchData();

// ---------------------------------------------------------------------------------------------

async function deleteData(id) {
    try {
        const con = mysql.createConnection(conData);

        con.connect(function(err) {
            if (err) throw err;

            const sql = 'DELETE FROM testTable WHERE id = ?';
            const values = [id];

            con.query(sql, values, function (err, result) {
                if (err) throw err;
                console.log(`Deleted record with ID: ${id}`);
                con.end();
            });
        });
    } catch (error) {
        console.error(error);
    }
}

deleteData(1);
fetchData();
