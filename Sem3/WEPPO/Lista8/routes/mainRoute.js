const express = require('express');
const router = express.Router();
const dropdownListRoute = require('./dropdownList');

router.get('/', (req, res) => {
  const selectedValue = 'Option 3';
  const options = ['Option 1', 'Option 2', 'Option 3'];
  res.render('mainRouteView', {options, selectedValue});
});

router.use('/dropdownList', dropdownListRoute);

module.exports = router;