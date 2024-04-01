// Light theme
const lightTheme = {
    backgroundColor: '#fff',
    textColor: '#333',
    buttonHoverColor: '#555',
    contentCardBackgroundColor: '#eee',
    teamMemberBackgroundColor: '#f5f5f5',
    blogPostBackgroundColor: '#f0f0f0',
    blogPostButtonBackgroundColor: '#4caf50',
    blogPostButtonHoverBackgroundColor: '#45a049',
    contactFormBackgroundColor: '#f9f9f9',
    contactFormBorderColor: '#ddd',
    contactFormTextareaColor: '#ccc',
};

// Dark theme
const darkTheme = {
    ...lightTheme,
    backgroundColor: '#111',
    teamMemberBackgroundColor: '#444',
    blogPostBackgroundColor: '#222',
    contactFormTextareaColor: '#666',
};

export { lightTheme, darkTheme };