import "./App.scss";
import { ThemeProvider } from "./providers/Theme";
import ThemeWrapper from "./components/ThemeWrapper/ThemeWrapper";
// import { createTheme, ThemeProvider as MuiThemeProvider } from "@mui/material";

import {
    RouterProvider,
    createBrowserRouter,
} from "react-router-dom";

import RootLayout from "./layouts/RootLayout/RootLayout";
import About from "./routes/About/About";
import Projects from "./routes/Projects/Projects";
import Contact from "./routes/Contact/Contact";

const router = createBrowserRouter([
    {
        path: "/",
        element: <RootLayout />,
        children: [
            {
                path: "/",
                element: <About />,
            },
            {
                path: "/about",
                element: <About />,
            },
            {
                path: "/projects",
                element: <Projects />,
            },
            {
                path: "/contact",
                element: <Contact />,
            },
        ],
    },
]);

// import { grey } from '@mui/material/colors';
// const muitheme = createTheme({
//     palette: {
//         primary: {
//             main: grey[900],
//         },
//     },
// });

function App() {
    return (
        <ThemeProvider>
            <ThemeWrapper>
                {/* <MuiThemeProvider theme={muitheme}> */}
                    <RouterProvider router={router} />
                {/* </MuiThemeProvider> */}
            </ThemeWrapper>
        </ThemeProvider>
    );
}

export default App;
