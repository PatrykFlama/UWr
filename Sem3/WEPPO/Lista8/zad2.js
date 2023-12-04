const express = require('express');
const path = require('path');
const mainRoute = require('./routes/mainRoute');

const app = express();
const port = 3000;
app.set('view engine', 'ejs');

app.set('views', path.join(__dirname, 'views'));

app.use('/', mainRoute);

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});