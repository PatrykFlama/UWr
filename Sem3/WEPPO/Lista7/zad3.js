const express = require('express')
const bodyParser = require('body-parser');
const app = express()
const port = 3000

app.set('view engine', 'ejs');      // default recommended view engine for Express
app.use(bodyParser.urlencoded({ extended: true }));     // for parsing data from forms

app.get('/', (req, res) => {
    res.render('form');
})

app.post('/submit', (req, res) => {
    const { firstName, lastName, subject, tasks } = req.body;

    // check if all required fields are filled
    if (!firstName || !lastName || !subject) {
        res.render('form', { error: 'Uzupełnij wszystkie pola!' });
        return;
    }

    // convert tasks to array
    const tasksArray = Array.isArray(tasks) ? tasks.map(Number) : [Number(tasks)];

    // render result in print view
    res.render('print', {
        firstName,
        lastName,
        subject,
        tasks: tasksArray,
    });
})

app.get('/print', (req, res) => {
    res.render('print');
})


app.listen(port, () => {
    console.log(`Example app listening on port ${port}`)
})
