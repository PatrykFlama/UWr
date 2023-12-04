const express = require('express');
const app = express();
const port = 3000;
app.set('view engine', 'ejs');

app.get('/', (req, res) => {
    res.setHeader('Content-Disposition', 'attachment; filename=example.txt');
    res.send('Hello World!');
});


app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
