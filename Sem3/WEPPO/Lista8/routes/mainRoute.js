const express = require('express');
const router = express.Router();
const dropdownListRoute = require('./dropdownList');

router.get('/', (req, res) => {
  const selectedValue = 'Option 2';
  res.render('mainView', {selectedValue});
});

router.use('/dropdownList', dropdownListRoute);

module.exports = router;