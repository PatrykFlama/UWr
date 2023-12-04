const express = require('express');
const router = express.Router();

router.get('/:selectedValue?', (req, res) => {
  const options = ['Option 1', 'Option 2', 'Option 3'];
  const selectedValue = req.params.selectedValue || 'Option 2';

  res.render('dropdownList', {options, selectedValue});
});

module.exports = router;