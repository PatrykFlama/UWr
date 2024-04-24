/** @type {import('tailwindcss').Config} */
export default {
  darkMode: 'selector',
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
    theme: {
      extend: {
        colors: {
          lightTheme: {
            background: '#fff',
            text: '#333',
            buttonHover: '#555',
            contentCardBackground: '#eee',
            teamMemberBackground: '#f5f5f5',
            blogPostBackground: '#f0f0f0',
            blogPostButtonBackground: '#4caf50',
            blogPostButtonHoverBackground: '#45a049',
            contactFormBackground: '#f9f9f9',
            contactFormBorder: '#ddd',
            contactFormTextarea: '#ccc',
          },
          darkTheme: {
            background: '#111',
            teamMemberBackground: '#444',
            blogPostBackground: '#222',
            contactFormTextarea: '#666',
          },
        },
      },
    },
  plugins: [],
}

