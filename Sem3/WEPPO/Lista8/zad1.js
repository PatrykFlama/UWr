const express = require('express');
const multer = require('multer');

const app = express();
const upload = multer({ dest: 'uploads/' });        // setup multer middleware for POST file uploads
app.set('view engine', 'ejs');

app.get('/', (req, res) => {
    res.render('upload_file');
});

app.post('/upload', upload.single('file'), (req, res) => {
    const file = req.file;
    console.log(file);

    res.send('File uploaded successfully');
});

app.listen(3000, () => {
    console.log('Server started on port 3000');
});
