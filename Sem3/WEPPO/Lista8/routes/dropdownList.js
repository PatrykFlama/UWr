const express = require('express');
const router = express.Router();

router.get('/:selectedValue?', (req, res) => {
  res.render('dropdownListView', {options, selectedValue});
});

module.exports = router;